------------------------------------------------------------------------------
--                                                                          --
--     Deepend - Dynamic Storage Pools for Ada 95, 2005, 2012, and 2022     --
--                                                                          --
--                        D Y N A M I C   P O O L S                         --
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

package body Dynamic_Pools is

   procedure Free_Storage_Element (Position : Storage_Vector.Cursor);

   procedure Free_Storage_Array is new Ada.Unchecked_Deallocation
     (Object => System.Storage_Elements.Storage_Array,
      Name => Storage_Array_Access);

   procedure Free_Subpool is new Ada.Unchecked_Deallocation
     (Object => Dynamic_Subpool,
      Name => Dynamic_Subpool_Access);

   function Storage_Size
     (Subpool : not null Dynamic_Subpool_Access)
      return Storage_Elements.Storage_Count;

   function Storage_Used
     (Subpool : not null Dynamic_Subpool_Access)
      return Storage_Elements.Storage_Count;

   protected body Subpool_Set is

      procedure Add (Subpool : Dynamic_Subpool_Access) is
      begin
         Subpools.Append (Subpool);
      end Add;

      --------------------------------------------------------------

      procedure Deallocate_All
      is
         procedure Deallocate_Subpools (Position : Subpool_Vector.Cursor) is
            Subpool : Dynamic_Subpool_Access :=
              Subpool_Vector.Element (Position);
         begin

            Subpool.Used_List.Iterate
              (Process => Free_Storage_Element'Access);

            Subpool.Free_List.Iterate
              (Process => Free_Storage_Element'Access);

            Free_Storage_Array (Subpool.Active);

            Free_Subpool (Subpool);
         end Deallocate_Subpools;
      begin
         Subpools.Iterate (Deallocate_Subpools'Access);
      end Deallocate_All;

      --------------------------------------------------------------

      procedure Delete (Subpool : Dynamic_Subpool_Access) is
         Position : Subpool_Vector.Cursor := Subpools.Find (Subpool);
      begin
         pragma Warnings (Off, "*Position*modified*but*n* referenced*");
         Subpools.Delete (Position);
         pragma Warnings (On, "*Position*modified*but*n* referenced*");
      end Delete;

      --------------------------------------------------------------

      function Storage_Total return Storage_Elements.Storage_Count
      is
         Result : Storage_Elements.Storage_Count := 0;

         procedure Subpool_Storage_Total (Position : Subpool_Vector.Cursor)
         is
            Subpool : constant Dynamic_Subpool_Access :=
              Subpool_Vector.Element (Position);
         begin
            Result := Result + Storage_Size (Subpool);
         end Subpool_Storage_Total;
      begin
         Subpools.Iterate (Subpool_Storage_Total'Access);
         return Result;
      end Storage_Total;

      --------------------------------------------------------------

      function Storage_Usage return Storage_Elements.Storage_Count
      is
         Result : Storage_Elements.Storage_Count := 0;

         procedure Subpool_Storage_Usage (Position : Subpool_Vector.Cursor)
         is
            Subpool : constant Dynamic_Subpool_Access :=
              Subpool_Vector.Element (Position);
         begin
            Result := Result + Storage_Used (Subpool);
         end Subpool_Storage_Usage;
      begin
         Subpools.Iterate (Subpool_Storage_Usage'Access);
         return Result;
      end Storage_Usage;

   end Subpool_Set;

   --------------------------------------------------------------

   overriding
   procedure Allocate
     (Pool : in out Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      Pool.Allocate_From_Subpool
        (Storage_Address,
         Size_In_Storage_Elements,
         Alignment,
         Pool.Default_Subpool_For_Pool);
   end Allocate;

   --------------------------------------------------------------

   overriding
   procedure Allocate_From_Subpool
     (Pool : in out Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Subpool : not null Subpool_Handle)
   is
      pragma Unreferenced (Pool);
      use type Ada.Containers.Count_Type;
      Sub : Dynamic_Subpool renames Dynamic_Subpool (Subpool.all);

      Next_Address : System.Address :=
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

      Alignment_Offset : System.Storage_Elements.Storage_Offset
        := Get_Alignment_Offset;

      Remaining : constant System.Storage_Elements.Storage_Count :=
        Sub.Active'Length - Sub.Next_Allocation;

   begin --  Allocate_From_Subpool

      pragma Assert (Is_Owner (Subpool, Current_Task));

      --  If there's not enough space in the current hunk of memory
      if Size_In_Storage_Elements + Alignment_Offset > Remaining then

         Sub.Used_List.Append (New_Item => Sub.Active);

         if Sub.Free_List.Length > 0 and then
           Sub.Free_List.First_Element'Length >= Size_In_Storage_Elements
         then
            Sub.Active := Sub.Free_List.First_Element;
            Sub.Free_List.Delete_First;
         else
            Sub.Active := new System.Storage_Elements.Storage_Array
              (1 .. Storage_Elements.Storage_Count'Max
                 (Size_In_Storage_Elements, Sub.Block_Size));
         end if;

         Sub.Next_Allocation := Sub.Active'First;
         Next_Address := Sub.Active (Sub.Next_Allocation)'Address;
         Alignment_Offset := Get_Alignment_Offset;

      end if;

      Storage_Address := Next_Address + Alignment_Offset;
      Sub.Next_Allocation :=
        Sub.Next_Allocation + Size_In_Storage_Elements + Alignment_Offset;

   end Allocate_From_Subpool;

   --------------------------------------------------------------

   procedure Create_Default_Subpool
     (Pool : in out Dynamic_Pool) is
   begin
      Pool.Default_Subpool := Pool.Create_Subpool;
   end Create_Default_Subpool;

   --------------------------------------------------------------

   overriding
   function Create_Subpool
     (Pool : not null access Dynamic_Pool) return not null Subpool_Handle is
   begin

      if Pool.Default_Block_Size = 0 then
         return Create_Subpool (Pool, Default_Allocation_Block_Size);
      else
         return Create_Subpool (Pool, Pool.Default_Block_Size);
      end if;

   end Create_Subpool;

   --------------------------------------------------------------

   not overriding
   function Create_Subpool
     (Pool : not null access Dynamic_Pool;
      Block_Size : Storage_Elements.Storage_Count)
      return not null Subpool_Handle
   is
      New_Pool : constant Dynamic_Subpool_Access
        := new Dynamic_Subpool'
          (Storage_Pools.Subpools.Root_Subpool with
           Block_Size => Block_Size,
           Used_List => <>,
           Free_List => <>,
           Active => new System.Storage_Elements.Storage_Array
             (1 .. Block_Size),
           Next_Allocation => 1,
           Owner => Ada.Task_Identification.Current_Task);

      Result : constant Subpool_Handle := Subpool_Handle (New_Pool);
   begin  --  Create_Subpool

      Pool.Subpools.Add (New_Pool);

      Storage_Pools.Subpools.Set_Pool_Of_Subpool
        (Subpool => Result,
         To => Pool.all);

      return Result;

   end Create_Subpool;

   --------------------------------------------------------------

   function Create_Subpool
     (Pool : not null access Dynamic_Pool;
      Block_Size : Storage_Elements.Storage_Count :=
        Default_Allocation_Block_Size) return Scoped_Subpool
   is
      New_Subpool : constant Subpool_Handle :=
        Create_Subpool (Pool, Block_Size);
   begin
      return  Result : Scoped_Subpool (Handle => New_Subpool);
   end Create_Subpool;

   --------------------------------------------------------------

   overriding
   procedure Deallocate_Subpool
     (Pool : in out Dynamic_Pool;
      Subpool : in out Subpool_Handle)
   is
      The_Subpool : Dynamic_Subpool_Access := Dynamic_Subpool_Access (Subpool);
      use type Storage_Pools.Subpools.Subpool_Handle;
   begin

      --  Only removes the access value from the Subpools container
      --  Does not actually delete the object which we still have a
      --  reference to above
      Pool.Subpools.Delete (The_Subpool);

      The_Subpool.Used_List.Iterate
        (Process => Free_Storage_Element'Access);

      The_Subpool.Used_List.Clear;

      The_Subpool.Free_List.Iterate
        (Process => Free_Storage_Element'Access);

      The_Subpool.Free_List.Clear;
      Free_Storage_Array (The_Subpool.Active);

      --  Handle case when deallocating the default pool
      --  Should only occur if client attempts to obtain the default
      --  subpool, then calls Unchecked_Deallocate_Subpool on that object
      if Pool.Default_Subpool /= null and then Subpool = Pool.Default_Subpool
      then
         Pool.Default_Subpool := null;
      end if;

      Free_Subpool (The_Subpool);

   end Deallocate_Subpool;

   --------------------------------------------------------------

   overriding
   function Default_Subpool_For_Pool
     (Pool : not null access Dynamic_Pool)
      return not null Subpool_Handle is
   begin
      return Pool.Default_Subpool;
   end Default_Subpool_For_Pool;

   --------------------------------------------------------------

   overriding
   procedure Finalize   (Pool : in out Dynamic_Pool) is
   begin
      Pool.Subpools.Deallocate_All;
   end Finalize;

   --------------------------------------------------------------

   procedure Free_Storage_Element (Position : Storage_Vector.Cursor) is
       Storage : Storage_Array_Access := Storage_Vector.Element (Position);
   begin
      Free_Storage_Array (Storage);
   end Free_Storage_Element;

   --------------------------------------------------------------

   function Has_Default_Subpool
     (Pool : Dynamic_Pool) return Boolean
   is
      use type Subpool_Handle;
   begin
      return (Pool.Default_Subpool /= null);
   end Has_Default_Subpool;

   --------------------------------------------------------------

   package body Scoped_Subpools is
      overriding
      procedure Finalize (Subpool : in out Scoped_Subpool) is
         Handle : Subpool_Handle := Subpool.Handle;
      begin
         --  Since Ada.Unchecked_Deallocate_Subpool doesn't exist in Ada 95,
         --  dispatch to Deallocate_Subpool directly.
         Storage_Pools.Subpools.Pool_Of_Subpool
           (Handle).Deallocate_Subpool (Handle);

      end Finalize;
   end Scoped_Subpools;

   --------------------------------------------------------------

   overriding procedure Initialize (Pool : in out Dynamic_Pool) is
   begin
      if Pool.Default_Block_Size > 0 then
         Pool.Default_Subpool := Pool.Create_Subpool;
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
     (Subpool : not null Subpool_Handle;
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
     (Subpool : not null Subpool_Handle;
      T : Task_Id := Current_Task) is
   begin

      pragma Assert
        ((Is_Owner (Subpool, Null_Task_Id) and then T = Current_Task)
         or else (Is_Owner (Subpool) and then T = Null_Task_Id));

      Dynamic_Subpool (Subpool.all).Owner := T;

   end Set_Owner;

   --------------------------------------------------------------

   function Storage_Size
     (Subpool : not null Dynamic_Subpool_Access)
      return Storage_Elements.Storage_Count
   is
      Result : Storage_Elements.Storage_Count := 0;

      procedure Add_Storage_Count (Position : Storage_Vector.Cursor) is
      begin
         Result := Result + Storage_Vector.Element (Position)'Length;
      end Add_Storage_Count;

   begin
      Subpool.Used_List.Iterate
        (Process => Add_Storage_Count'Access);

      Subpool.Free_List.Iterate
        (Process => Add_Storage_Count'Access);

      return Result + Subpool.Active'Length;
   end Storage_Size;

   --------------------------------------------------------------

   function Storage_Size
     (Subpool : not null Subpool_Handle) return Storage_Elements.Storage_Count
   is
      The_Subpool : constant Dynamic_Subpool_Access
        := Dynamic_Subpool_Access (Subpool);
   begin
      return Storage_Size (The_Subpool);
   end Storage_Size;

   --------------------------------------------------------------

   overriding
   function Storage_Size
     (Pool : Dynamic_Pool) return Storage_Elements.Storage_Count is
   begin
      return Pool.Subpools.Storage_Total;
   end Storage_Size;

   --------------------------------------------------------------

   function Storage_Used
     (Subpool : not null Dynamic_Subpool_Access)
      return Storage_Elements.Storage_Count
   is
      Result : Storage_Elements.Storage_Count := 0;

      procedure Add_Storage_Count (Position : Storage_Vector.Cursor) is
      begin
         Result := Result + Storage_Vector.Element (Position)'Length;
      end Add_Storage_Count;

   begin
      Subpool.Used_List.Iterate
        (Process => Add_Storage_Count'Access);

      return Result + Subpool.Next_Allocation - 1;
   end Storage_Used;

   --------------------------------------------------------------

   function Storage_Used
     (Subpool : not null Subpool_Handle) return Storage_Elements.Storage_Count
   is
      The_Subpool : constant Dynamic_Subpool_Access
        := Dynamic_Subpool_Access (Subpool);
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
      use type Storage_Pools.Subpools.Subpool_Handle;
   begin
      if Subpool = null then
         return;
      end if;

      --  Since Ada.Unchecked_Deallocate_Subpool doesn't exist in Ada 2005,
      --  dispatch to Deallocate_Subpool directly.
      Storage_Pools.Subpools.Pool_Of_Subpool
        (Subpool).Deallocate_Subpool (Subpool);

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

         Storage_Pools.Subpools.Pool_Of_Subpool (Subpool).Allocate_From_Subpool
           (Storage_Address => Location,
            Size_In_Storage_Elements =>
              Value'Size / System.Storage_Elements.Storage_Element'Size,
            Alignment       => Allocation_Type'Alignment,
            Subpool         => Subpool);

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

         Storage_Pools.
           Subpools.Pool_Of_Subpool (Subpool.Handle).Allocate_From_Subpool
           (Storage_Address => Location,
            Size_In_Storage_Elements =>
              Value'Size / System.Storage_Elements.Storage_Element'Size,
            Alignment       => Allocation_Type'Alignment,
            Subpool         => Subpool.Handle);

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

end Dynamic_Pools;
