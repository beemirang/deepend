------------------------------------------------------------------------------
--                                                                          --
--     Deepend - Dynamic Storage Pools for Ada 95, 2005, 2012, and 2022     --
--                                                                          --
--                 B O U N D E D   D Y N A M I C   P O O L S                --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                Copyright (C) 2011-2024, Bradley J. Moore                 --
--                                                                          --
-- Deepend is free software;  you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License as published  by the Free --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version.  Deepend is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the  implied warranty of             --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body Bounded_Dynamic_Pools is

   function Storage_Size
     (Subpool : Dynamic_Subpool_Access)
      return Storage_Elements.Storage_Count;

   function Storage_Used
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

      function Storage_Total return Storage_Elements.Storage_Count
      is
         Result : Storage_Elements.Storage_Count := 0;
      begin
         for I in 1 .. Subpools.Last loop
            Result := Result + Storage_Size (Subpools.Subpool_List (I));
         end loop;

         return Result;
      end Storage_Total;

      --------------------------------------------------------------

      function Storage_Usage return Storage_Elements.Storage_Count
      is
         Result : Storage_Elements.Storage_Count := 0;
      begin
         for I in 1 .. Subpools.Last loop
            Result := Result + Storage_Used (Subpools.Subpool_List (I));
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
         Default_Subpool_For_Pool (Pool'Access));

   end Allocate;

   --------------------------------------------------------------

   procedure Allocate_From_Subpool
     (Pool : in out Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Subpool : Subpool_Handle)
   is
      pragma Unreferenced (Pool);
      Sub : Dynamic_Subpool renames Dynamic_Subpool (Subpool.all);

      Next_Address : constant System.Address :=
        Sub.Active (Sub.Next_Allocation)'Address;

      function Get_Alignment_Offset
        return System.Storage_Elements.Storage_Offset is
      begin
         if Next_Address mod Alignment = 0 then
            return 0;
         else
            return Alignment - (Next_Address mod Alignment);
         end if;
      end Get_Alignment_Offset;

      Alignment_Offset : constant System.Storage_Elements.Storage_Offset
        := Get_Alignment_Offset;

      Remaining : constant System.Storage_Elements.Storage_Count :=
        Sub.Active'Length - Sub.Next_Allocation;

   begin --  Allocate_From_Subpool

      pragma Assert (Is_Owner (Subpool, Current_Task));

      --  If there's not enough space in the current hunk of memory
      if Size_In_Storage_Elements + Alignment_Offset > Remaining then
         raise Storage_Error;
      end if;

      Storage_Address := Next_Address + Alignment_Offset;

      Sub.Next_Allocation :=
        Sub.Next_Allocation + Size_In_Storage_Elements + Alignment_Offset;

   end Allocate_From_Subpool;

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

   procedure Create_Default_Subpool
     (Pool : in out Dynamic_Pool) is
   begin
      Pool.Default_Subpool := Create_Subpool (Pool'Access);
   end Create_Default_Subpool;

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

      Result : constant Subpool_Handle := Subpool_Handle (New_Subpool);
   begin

      New_Subpool.Next_Allocation := 1;
      New_Subpool.Owner := Ada.Task_Identification.Current_Task;
      New_Subpool.Reclaimed := False;

      Pool.Subpools.Add (New_Subpool);

      Sys.Storage_Pools.Subpools.Set_Pool_Of_Subpool
        (Subpool => Result,
         To => Pool.all);

      return Result;

   end Create_Subpool;

   --------------------------------------------------------------

   procedure Deallocate_Subpool
     (Pool : in out Dynamic_Pool;
      Subpool : in out Subpool_Handle)
   is
      The_Subpool : Dynamic_Subpool_Access := Dynamic_Subpool_Access (Subpool);
      use type Sys.Storage_Pools.Subpools.Subpool_Handle;
   begin

      --  Only removes the access value from the Subpools container
      --  Does not actually delete the object which we still have a
      --  reference to above
      Pool.Subpools.Delete (The_Subpool);

      The_Subpool.Next_Allocation := 1;

      --  Handle case when deallocating the default pool
      --  Should only occur if client attempts to obtain the default
      --  subpool, then calls Unchecked_Deallocate_Subpool on that object
      if Pool.Default_Subpool /= null and then Subpool = Pool.Default_Subpool
      then
         Pool.Default_Subpool := null;
      end if;

      if The_Subpool.Reusable then
         The_Subpool.Reclaimed := True;
      else
         Free_Subpool (The_Subpool);
      end if;

   end Deallocate_Subpool;

   --------------------------------------------------------------

   function Default_Subpool_For_Pool
     (Pool : access Dynamic_Pool)
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
      Deallocate_Subpool (Subpool.Pool.all, Subpool.Handle);
   end Finalize;

   --------------------------------------------------------------

   function Handle
     (Subpool : Scoped_Subpool) return Subpool_Handle is
   begin
      return Subpool.Handle;
   end Handle;

   --------------------------------------------------------------

   function Has_Default_Subpool
     (Pool : Dynamic_Pool) return Boolean
   is
      use type Subpool_Handle;
   begin
      return (Pool.Default_Subpool /= null);
   end Has_Default_Subpool;

   --------------------------------------------------------------

   procedure Initialize (Subpool : in out Scoped_Subpool) is
   begin

      if Subpool.Heap_Allocated then
         Subpool.Handle := Create_Subpool (Subpool.Pool, Subpool.Size);
      else

         Subpool.Handle := Subpool.Storage'Unchecked_Access;
         Subpool.Storage.Next_Allocation := 1;
         Subpool.Storage.Owner := Ada.Task_Identification.Current_Task;
         Subpool.Storage.Reclaimed := False;

         Subpool.Pool.Subpools.Add (Subpool.Storage'Unchecked_Access);

         Sys.Storage_Pools.Subpools.Set_Pool_Of_Subpool
           (Subpool => Subpool.Handle,
            To => Subpool.Pool.all);

      end if;

   end Initialize;

   --------------------------------------------------------------

   procedure Initialize (Pool : in out Dynamic_Pool) is
   begin
      Pool.Subpools.Initialize;

      if Pool.Default_Subpool_Size > 0 then
         Pool.Default_Subpool := Create_Subpool (Pool'Access);
      else
         Pool.Default_Subpool := null;
      end if;

      Pool.Owner := Ada.Task_Identification.Current_Task;
   end Initialize;

   --------------------------------------------------------------

   function Is_Owner
     (Pool : Dynamic_Pool;
      T : Task_Id := Current_Task) return Boolean is
   begin
      return (Pool.Owner = T);
   end Is_Owner;

   --------------------------------------------------------------

   function Is_Owner
     (Subpool : Subpool_Handle;
      T : Task_Id := Current_Task) return Boolean is
   begin
      return (Dynamic_Subpool (Subpool.all).Owner = T);
   end Is_Owner;

   --------------------------------------------------------------

   procedure Set_Owner
     (Pool : in out Dynamic_Pool;
      T : Task_Id := Current_Task) is
   begin
      pragma Assert
        ((Is_Owner (Pool, Null_Task_Id) and then T = Current_Task)
         or else (Is_Owner (Pool) and then T = Null_Task_Id));

      Pool.Owner := T;

   end Set_Owner;

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
      return Subpool.Size;
   end Storage_Size;

   --------------------------------------------------------------

   function Storage_Size
     (Subpool : Subpool_Handle) return Storage_Elements.Storage_Count
   is
      The_Subpool : constant Dynamic_Subpool_Access :=
        Dynamic_Subpool_Access (Subpool);
   begin
      return Storage_Size (The_Subpool);
   end Storage_Size;

   --------------------------------------------------------------

   function Storage_Size
     (Pool : Dynamic_Pool) return Storage_Elements.Storage_Count is
   begin
      return Pool.Subpools.Storage_Total;
   end Storage_Size;

   --------------------------------------------------------------

   function Storage_Used
     (Subpool : Dynamic_Subpool_Access)
      return Storage_Elements.Storage_Count is
   begin
      return Subpool.Next_Allocation - 1;
   end Storage_Used;

   --------------------------------------------------------------

   function Storage_Used
     (Subpool : Subpool_Handle) return Storage_Elements.Storage_Count
   is
      The_Subpool : constant Dynamic_Subpool_Access :=
        Dynamic_Subpool_Access (Subpool);
   begin
      return Storage_Used (The_Subpool);
   end Storage_Used;

   --------------------------------------------------------------

   function Storage_Used
     (Pool : Dynamic_Pool) return Storage_Elements.Storage_Count is
   begin
      return Pool.Subpools.Storage_Usage;
   end Storage_Used;

   --------------------------------------------------------------

   procedure Unchecked_Deallocate_Subpool
     (Subpool : in out Subpool_Handle)
   is
      use type Sys.Storage_Pools.Subpools.Subpool_Handle;
   begin
      if Subpool = null then
         return;
      end if;

      --  Since Ada.Unchecked_Deallocate_Subpool doesn't exist in Ada 2005,
      --  dispatch to Deallocate_Subpool directly.
      Deallocate_Subpool
        (Dynamic_Pool (Sys.Storage_Pools.Subpools.Pool_Of_Subpool
         (Subpool).all),
         Subpool);

   end Unchecked_Deallocate_Subpool;

   package body Subpool_Allocators is

      function Convert is new
        Ada.Unchecked_Conversion (Source => System.Address,
                                  Target => Allocation_Type_Access);

      function Allocate
        (Subpool : Subpool_Handle;
         Value : Allocation_Type := Default_Value)
      return Allocation_Type_Access
      is
         Location : System.Address;
      begin

         Allocate_From_Subpool
           (Dynamic_Pool
              (Sys.Storage_Pools.Subpools.Pool_Of_Subpool (Subpool).all),
            Storage_Address => Location,
            Size_In_Storage_Elements =>
              Value'Size / System.Storage_Elements.Storage_Element'Size,
            Alignment => Allocation_Type'Alignment,
            Subpool => Subpool);

         declare
            Loc : constant System.Address := Location;
            Result : Allocation_Type;
            for Result'Address use Loc;
            pragma Import (C, Result);
         begin
            Result := Value;
            return Convert (Loc);
         end;

      end Allocate;

      --------------------------------------------------------------

      function Allocate
        (Subpool : Scoped_Subpool;
         Value   : Allocation_Type := Default_Value)
      return Allocation_Type_Access
      is
         Location : System.Address;
      begin

         Allocate_From_Subpool
           (Dynamic_Pool
              (Sys.Storage_Pools.Subpools.Pool_Of_Subpool
                   (Subpool.Handle).all),
            Storage_Address => Location,
            Size_In_Storage_Elements =>
              Value'Size / System.Storage_Elements.Storage_Element'Size,
            Alignment => Allocation_Type'Alignment,
            Subpool => Subpool.Handle);

         declare
            Loc : constant System.Address := Location;
            Result : Allocation_Type;
            for Result'Address use Loc;
            pragma Import (C, Result);
         begin
            Result := Value;
            return Convert (Loc);
         end;

      end Allocate;

   end Subpool_Allocators;

end Bounded_Dynamic_Pools;
