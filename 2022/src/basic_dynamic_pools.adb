------------------------------------------------------------------------------
--                                                                          --
--     Deepend - Dynamic Storage Pools for Ada 95, 2005, 2012, and 2022     --
--                                                                          --
--                   B A S I C   D Y N A M I C   P O O L S                  --
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

package body Basic_Dynamic_Pools is

   procedure Free_Storage_Element (Position : Storage_Vector.Cursor);

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
      Next_Address : System.Address :=
        Pool.Active (Pool.Next_Allocation)'Address;

      Alignment_Offset : System.Storage_Elements.Storage_Offset
        := (if Next_Address mod Alignment = 0 then 0
            else Alignment - (Next_Address mod Alignment));

      Remaining : constant System.Storage_Elements.Storage_Count :=
        Pool.Active'Length - Pool.Next_Allocation;

      use type Ada.Containers.Count_Type;

   begin --  Allocate

      --  If there's not enough space in the current hunk of memory
      if Size_In_Storage_Elements + Alignment_Offset > Remaining then

         Pool.Used_List.Append (New_Item => Pool.Active);

         if Pool.Free_List.Length > 0 and then
           Pool.Free_List.First_Element'Length >= Size_In_Storage_Elements
         then
            Pool.Active := Pool.Free_List.First_Element;
            Pool.Free_List.Delete_First;
         else
            Pool.Active := new System.Storage_Elements.Storage_Array
              (1 .. Storage_Elements.Storage_Count'Max
                 (Size_In_Storage_Elements, Pool.Block_Size));
         end if;

         Pool.Next_Allocation := Pool.Active'First;
         Next_Address := Pool.Active (Pool.Next_Allocation)'Address;
         Alignment_Offset := (if Next_Address mod Alignment = 0 then 0
                              else Alignment - (Next_Address mod Alignment));
      end if;

      Storage_Address := Next_Address + Alignment_Offset;

      Pool.Next_Allocation := @ + Size_In_Storage_Elements + Alignment_Offset;
   end Allocate;

   --------------------------------------------------------------

   overriding
   procedure Finalize   (Pool : in out Basic_Dynamic_Pool) is
   begin
      Pool.Used_List.Iterate (Process => Free_Storage_Element'Access);
      Pool.Free_List.Iterate (Process => Free_Storage_Element'Access);
      Free_Storage_Array (Pool.Active);
   end Finalize;

   --------------------------------------------------------------

   procedure Free_Storage_Element (Position : Storage_Vector.Cursor) is
       Storage : Storage_Array_Access := Storage_Vector.Element (Position);
   begin
      Free_Storage_Array (Storage);
   end Free_Storage_Element;

   --------------------------------------------------------------

   overriding
   procedure Initialize (Pool : in out Basic_Dynamic_Pool) is
   begin
      Pool.Active := new System.Storage_Elements.Storage_Array
        (1 .. Pool.Block_Size);
      Pool.Next_Allocation := 1;
      Pool.Owner := Ada.Task_Identification.Current_Task;
   end Initialize;

   --------------------------------------------------------------

   procedure Set_Owner
     (Pool : in out Basic_Dynamic_Pool;
      T : Task_Id := Current_Task) is
   begin
      Pool.Owner := T;
   end Set_Owner;

   --------------------------------------------------------------

   overriding
   function Storage_Size
     (Pool : Basic_Dynamic_Pool)
      return Storage_Elements.Storage_Count
   is
      Result : Storage_Elements.Storage_Count := 0;
   begin

      for E in Pool.Used_List.Iterate loop
         Result := Result + Pool.Used_List (E).all'Length;
      end loop;

      for E in Pool.Free_List.Iterate loop
         Result := Result + Pool.Free_List (E).all'Length;
      end loop;

      return Result + Pool.Active'Length;
   end Storage_Size;

   --------------------------------------------------------------

   function Storage_Used
     (Pool : Basic_Dynamic_Pool)
      return Storage_Elements.Storage_Count
   is
      Result : Storage_Elements.Storage_Count := 0;
   begin

      for E in Pool.Used_List.Iterate loop
         Result := Result + Pool.Used_List (E).all'Length;
      end loop;

      return Result + Pool.Next_Allocation - 1;
   end Storage_Used;

end Basic_Dynamic_Pools;
