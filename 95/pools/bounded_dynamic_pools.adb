------------------------------------------------------------------------------
--
--              Deepend - Dynamic Pools for Ada 2005 and Ada 2012
--
--                        D Y N A M I C   P O O L S
--
--                                B o d y
--
--                  Copyright (C) 2011, Bradley J. Moore
--
--  Deepend is free software;  you can  redistribute it  and/or modify it
--  under  terms of the  GNU General Public License  as  published  by the
--  Free Software  Foundation;  either version 2,  or (at your option) any
--  later  version.  Paraffin is  distributed in the hope that it  will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE.  See the GNU
--  General Public License for  more details.  You should have  received a
--  copy of the GNU General Public License distributed with Deepend;  see
--  file  COPYING.  If  not,  write  to  the  Free  Software  Foundation,
--  51 Franklin  Street,  Fifth  Floor, Boston, MA 02110-1301, USA.
--
--  As a  special exception, if other files  instantiate generics from
--  this unit,  or you link this  unit with other files  to produce an
--  executable,  this unit  does  not by  itself  cause the  resulting
--  executable to be covered by  the GNU General Public License.  This
--  exception does  not however invalidate  any other reasons  why the
--  executable file might be covered by the GNU Public License.
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body Bounded_Dynamic_Pools is

   function Storage_Size
     (Subpool : Dynamic_Subpool_Access)
      return Storage_Elements.Storage_Count;

   procedure Free_Subpool is new Ada.Unchecked_Deallocation
     (Object => Dynamic_Subpool,
      Name => Dynamic_Subpool_Access);

   procedure Append
     (Container : in out Subpool_Vector;
      New_Item : Dynamic_Subpool_Access);

   protected body Subpool_Set is

      procedure Add (Subpool : Dynamic_Subpool_Access) is
      begin
         Append (Subpools, Subpool);
      end Add;

      --------------------------------------------------------------

      procedure Deallocate_All is
      begin

         for I in 1 .. Subpools.Last loop

            if Subpools.Subpool_List (I).Reusable then
               Subpools.Subpool_List (I).Reclaimed := True;
            else
               Free_Subpool (Subpools.Subpool_List (I));
            end if;

         end loop;

      end Deallocate_All;

      --------------------------------------------------------------

      procedure Delete (Subpool : Dynamic_Subpool_Access) is
      begin
         Delete_Loop : for I in 1 .. Subpools.Last loop
            if Subpools.Subpool_List (I) = Subpool then
               Subpools.Subpool_List (I .. Subpools.Last - 1)
                 := Subpools.Subpool_List (I + 1 .. Subpools.Last);
               Subpools.Last := Subpools.Last - 1;
               exit Delete_Loop;
            end if;
         end loop Delete_Loop;
      end Delete;

      --------------------------------------------------------------

      procedure Initialize is
      begin
         Subpools.Last := 0;
      end Initialize;
      --------------------------------------------------------------

      function Storage_Usage return Storage_Elements.Storage_Count
      is
         Result : Storage_Elements.Storage_Count := 0;
      begin
         for I in 1 .. Subpools.Last loop
            Result := Result + Storage_Size (Subpools.Subpool_List (I));
         end loop;

         return Result;
      end Storage_Usage;

   end Subpool_Set;

   --------------------------------------------------------------

   procedure Allocate
     (Pool : in out Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      Allocate_From_Subpool
        (Pool,
         Storage_Address,
         Size_In_Storage_Elements,
         Alignment,
         Pool.Default_Subpool_For_Pool);
   end Allocate;

   --------------------------------------------------------------

   procedure Allocate_From_Subpool
     (Pool : in out Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Subpool : Subpool_Handle)
   is
      pragma Unreferenced (Alignment, Pool);
      Sub : Dynamic_Subpool renames Dynamic_Subpool (Subpool.all);
   begin

      pragma Assert (Is_Owner (Subpool, Current_Task));

      --  If there's not enough space in the current hunk of memory
      if Size_In_Storage_Elements >
        Sub.Active'Length - Sub.Next_Allocation then

         raise Storage_Error;

      end if;

      Storage_Address := Sub.Active (Sub.Next_Allocation)'Address;
      Sub.Next_Allocation := Sub.Next_Allocation + Size_In_Storage_Elements;
   end Allocate_From_Subpool;

   --------------------------------------------------------------

   function Allocation
     (Subpool : Subpool_Handle) return Allocation_Type_Access
   is
      Location : System.Address;

      function Convert is new Ada.Unchecked_Conversion
        (Source => System.Address,
         Target => Allocation_Type_Access);
   begin

      Allocate_From_Subpool
        (Storage_Pools.Subpools.Pool_Of_Subpool (Subpool).all,
         Storage_Address => Location,
         Size_In_Storage_Elements =>
           Allocation_Type'Max_Size_In_Storage_Elements,
         Alignment => Allocation_Type'Alignment,
         Subpool => Subpool);

      return Convert (Location);
   end Allocation;

   --------------------------------------------------------------

   procedure Append
     (Container : in out Subpool_Vector;
      New_Item : Dynamic_Subpool_Access) is
   begin
      if Container.Last = Container.Subpool_List'Length then
          raise Storage_Error;
      end if;

      Container.Last := Container.Last + 1;
      Container.Subpool_List (Container.Last) := New_Item;
   end Append;

   --------------------------------------------------------------

   function Create_Subpool
     (Pool : access Dynamic_Pool) return Subpool_Handle is
   begin

      if Pool.Default_Subpool_Size = 0 then
         return Create_Subpool (Pool, Default_Subpool_Default_Size);
      else
         return Create_Subpool (Pool, Pool.Default_Subpool_Size);
      end if;

   end Create_Subpool;

   --------------------------------------------------------------

   function Create_Subpool
     (Pool : access Dynamic_Pool;
      Size : Storage_Elements.Storage_Count)
      return Subpool_Handle
   is
      New_Subpool : constant Dynamic_Subpool_Access
        := new Dynamic_Subpool
          (Size => Size,
           Reusable => False);

      Result : constant Subpool_Handle := New_Subpool.all'Unchecked_Access;
   begin

      New_Subpool.Reusable := False;
      New_Subpool.Next_Allocation := 1;
      New_Subpool.Owner := Ada.Task_Identification.Current_Task;
      New_Subpool.Reclaimed := False;

      Pool.Subpools.Add (New_Subpool);

      Storage_Pools.Subpools.Set_Pool_Of_Subpool
        (Subpool => Result,
         To => Pool.all);

      return Result;

   end Create_Subpool;

   --------------------------------------------------------------

   package body Scoped_Subpools is

      procedure Create_Subpool
        (Pool : access Dynamic_Pool;
         Size : Storage_Elements.Storage_Count;
         Heap_Allocated : Boolean := True;
         Subpool : out Scoped_Subpool) is
      begin
         if Heap_Allocated then
            Subpool :=-
              Finalization.Limited_Controlled with
              Size => Size,
              Heap_Allocated => True,
              Subpool => Create_Subpool (Pool, Size));

         else
            Subpool :=
              (Finalization.Limited_Controlled with
               Size => Size,
               Heap_Allocated => False,
               Subpool => null,
               Storage =>
                 (Storage_Pools.Subpools.Root_Subpool with
                  Size => Size,
                  Reusable => True,
                  Active => ,
                  Next_Allocation => 1,
                  Owner => Ada.Task_Identification.Current_Task,
                  Reclaimed => False))
            do

               Result.Subpool := Result.Storage'Unchecked_Access;

               Pool.Subpools.Add (Result.Storage'Unchecked_Access);

               Storage_Pools.Subpools.Set_Pool_Of_Subpool
                 (Subpool => Result.Subpool,
                  To => Pool.all);

            end return;
         end if;

      end Create_Subpool;

   end Scoped_Subpools;

   --------------------------------------------------------------

   procedure Deallocate_Subpool
     (Pool : in out Dynamic_Pool;
      Subpool : in out Subpool_Handle)
   is
      The_Subpool : Dynamic_Subpool_Access
        := Dynamic_Subpool (Subpool.all)'Access;

      use type Storage_Pools.Subpools.Subpool_Handle;
   begin

      --  Only removes the access value from the Subpools container
      --  Does not actually delete the object which we still have a
      --  reference to above
      Pool.Subpools.Delete (The_Subpool);

      The_Subpool.Next_Allocation := 1;

      --  Handle case when deallocating the default pool
      --  Should only occur if client attempts to obtain the default
      --  subpool, then calls Unchecked_Deallocate_Subpool on that object
      if Pool.Default_Subpool /= null and then
        Subpool = Pool.Default_Subpool then

         Pool.Default_Subpool :=
           Create_Subpool (Pool'Access,
                           Size => Pool.Default_Subpool_Size);
      end if;

      if The_Subpool.Reusable then
         The_Subpool.Reclaimed := True;
      else
         Free_Subpool (The_Subpool);
      end if;

   end Deallocate_Subpool;

   --------------------------------------------------------------

   function Default_Subpool_For_Pool
     (Pool : Dynamic_Pool)
      return Subpool_Handle is
   begin
      return Pool.Default_Subpool;
   end Default_Subpool_For_Pool;

   --------------------------------------------------------------

   procedure Finalize   (Pool : in out Dynamic_Pool) is
   begin
      Pool.Subpools.Deallocate_All;
   end Finalize;

   --------------------------------------------------------------

   procedure Finalize (Subpool : in out Scoped_Subpool) is
   begin
     --  Since Ada.Unchecked_Deallocate_Subpool doesn't exist in Ada 2005,
      --  dispatch to Deallocate_Subpool directly.
      Storage_Pools.Subpools.Pool_Of_Subpool
        (Subpool.Subpool).Deallocate_Subpool (Subpool.Subpool);

   end Finalize;

   --------------------------------------------------------------

   function Handle
     (Subpool : Scoped_Subpool) return Subpool_Handle is
   begin
      return Subpool.Subpool;
   end Handle;

   --------------------------------------------------------------

   procedure Initialize (Pool : in out Dynamic_Pool) is
   begin
      Pool.Subpools.Initialize;

      if Pool.Default_Subpool_Size > 0 then
         Pool.Default_Subpool := Create_Subpool (Pool'Access);
      else
         Pool.Default_Subpool := null;
      end if;
   end Initialize;

   --------------------------------------------------------------

   function Initialized_Allocation
     (Subpool : Subpool_Handle;
      Qualified_Expression : Allocation_Type)
      return Allocation_Type_Access
   is
      Location : System.Address;

      function Convert is new Ada.Unchecked_Conversion
        (Source => System.Address,
         Target => Allocation_Type_Access);
   begin

      Allocate_From_Subpool
        (Storage_Pools.Subpools.Pool_Of_Subpool (Subpool).all,
         Storage_Address => Location,
         Size_In_Storage_Elements =>
           Qualified_Expression'Size /
             System.Storage_Elements.Storage_Element'Size,
         Alignment => Allocation_Type'Alignment,
         Subpool => Subpool);

      declare
         Result : constant Allocation_Type_Access := Convert (Location);
      begin
         Result.all := Qualified_Expression;
         return Result;
      end;

   end Initialized_Allocation;

   --------------------------------------------------------------

   function Is_Owner
     (Subpool : Subpool_Handle;
      T : Task_Id := Current_Task) return Boolean is
   begin
      return (Dynamic_Subpool (Subpool.all).Owner = T);
   end Is_Owner;

   --------------------------------------------------------------

   procedure Set_Owner
     (Subpool : Subpool_Handle;
      T : Task_Id := Current_Task) is
   begin

      pragma Assert
        ((Is_Owner (Subpool, Null_Task_Id) and then T = Current_Task)
         or else (Is_Owner (Subpool) and then T = Null_Task_Id));

      Dynamic_Subpool (Subpool.all).Owner := T;

   end Set_Owner;

   --------------------------------------------------------------

   function Storage_Size
     (Subpool : Dynamic_Subpool_Access)
      return Storage_Elements.Storage_Count is
   begin
      return Subpool.Next_Allocation - 1;
   end Storage_Size;

   --------------------------------------------------------------

   function Storage_Size
     (Subpool : Subpool_Handle) return Storage_Elements.Storage_Count
   is
      The_Subpool : constant Dynamic_Subpool_Access :=
        Dynamic_Subpool (Subpool.all)'Access;
   begin
      return Storage_Size (The_Subpool);
   end Storage_Size;

   --------------------------------------------------------------

   function Storage_Size
     (Pool : Dynamic_Pool) return Storage_Elements.Storage_Count is
   begin
      return Pool.Subpools.Storage_Usage;
   end Storage_Size;

   --------------------------------------------------------------

   procedure Unchecked_Deallocate_Subpool
     (Subpool : in out Subpool_Handle)
   is
      use type Storage_Pools.Subpools.Subpool_Handle;
   begin
      if Subpool = null then
         return;
      end if;

      --  Since Ada.Unchecked_Deallocate_Subpool doesn't exist in Ada 2005,
      --  dispatch to Deallocate_Subpool directly.
      Deallocate_Subpool
         (Storage_Pools.Subpools.Pool_Of_Subpool (Subpool),
          Subpool);

   end Unchecked_Deallocate_Subpool;

end Bounded_Dynamic_Pools;