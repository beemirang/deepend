------------------------------------------------------------------------------
--                                                                          --
--     Deepend - Dynamic Storage Pools for Ada 95, 2005, 2012, and 2022     --
--                                                                          --
--           B A S I C   B O U N D E D   D Y N A M I C   P O O L S          --
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

package body Basic_Bounded_Dynamic_Pools is

   procedure Free_Storage_Array is new Ada.Unchecked_Deallocation
     (Object => Storage_Array,
      Name => Storage_Array_Access);

   --------------------------------------------------------------

   overriding
   procedure Allocate
     (Pool : in out Basic_Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      function Get_Next_Address return System.Address is
      begin
         if Pool.Heap_Allocated then
            return Pool.Active_Access (Pool.Next_Allocation)'Address;
         else
            return Pool.Active (Pool.Next_Allocation)'Address;
         end if;
      end Get_Next_Address;

      Next_Address : constant System.Address := Get_Next_Address;

      use type System.Storage_Elements.Storage_Count;

      function Get_Alignment_Offset
        return System.Storage_Elements.Storage_Offset
      is
      begin
         if Next_Address mod Alignment = 0 then
            return 0;
         else
            return Alignment - (Next_Address mod Alignment);
         end if;
      end Get_Alignment_Offset;

      Alignment_Offset : constant System.Storage_Elements.Storage_Offset
        := Get_Alignment_Offset;

      function Get_Remaining return System.Storage_Elements.Storage_Count is
      begin
         if Pool.Heap_Allocated then
            return Pool.Active_Access'Length - Pool.Next_Allocation;
         else
            return Pool.Active'Length - Pool.Next_Allocation;
         end if;
      end Get_Remaining;

      Remaining : constant System.Storage_Elements.Storage_Count :=
        Get_Remaining;

   begin -- Allocate

      pragma Assert (Is_Owner (Pool, Current_Task));

      if Size_In_Storage_Elements + Alignment_Offset > Remaining then

         raise Storage_Error with
           "Size" &
           System.Storage_Elements.Storage_Count'Image
             (Size_In_Storage_Elements) & " > remaining" &
           System.Storage_Elements.Storage_Offset'Image
             (Remaining - Alignment_Offset);
      end if;

      Storage_Address := Next_Address + Alignment_Offset;

      Pool.Next_Allocation := Pool.Next_Allocation +
        Size_In_Storage_Elements + Alignment_Offset;

   end Allocate;

   --------------------------------------------------------------

   overriding
   procedure Finalize   (Pool : in out Basic_Dynamic_Pool) is
   begin
      if Pool.Heap_Allocated then
         Free_Storage_Array (Pool.Active_Access);
      end if;
   end Finalize;

   --------------------------------------------------------------

   overriding
   procedure Initialize (Pool : in out Basic_Dynamic_Pool) is
   begin
      if Pool.Heap_Allocated then
         Pool.Active_Access := new System.Storage_Elements.Storage_Array
           (1 .. Pool.Size);
      end if;

      Pool.Next_Allocation := 1;
      Pool.Owner := Ada.Task_Identification.Current_Task;
   end Initialize;

   --------------------------------------------------------------

   function Is_Owner
     (Pool : Basic_Dynamic_Pool;
      T : Task_Id := Current_Task) return Boolean is
   begin
      return (Pool.Owner = T);
   end Is_Owner;

   --------------------------------------------------------------

   procedure Set_Owner
     (Pool : in out Basic_Dynamic_Pool;
      T : Task_Id := Current_Task) is
   begin
      pragma Assert
        ((Is_Owner (Pool, Null_Task_Id) and then T = Current_Task)
         or else (Is_Owner (Pool) and then T = Null_Task_Id));

      Pool.Owner := T;

   end Set_Owner;

   --------------------------------------------------------------

   overriding
   function Storage_Size
     (Pool : Basic_Dynamic_Pool)
      return Storage_Elements.Storage_Count is
   begin
      return Pool.Size;
   end Storage_Size;

   --------------------------------------------------------------

   function Storage_Used
     (Pool : Basic_Dynamic_Pool)
      return Storage_Elements.Storage_Count is
   begin
      return Pool.Next_Allocation - 1;
   end Storage_Used;

end Basic_Bounded_Dynamic_Pools;
