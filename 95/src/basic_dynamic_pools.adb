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

   procedure Free_Storage_Array is new Ada.Unchecked_Deallocation
     (Object => System.Storage_Elements.Storage_Array,
      Name => Storage_Array_Access);

   procedure Free_Vector is new Ada.Unchecked_Deallocation
     (Object => Vector,
      Name => Vector_Access);

   procedure Append (Container : in out Storage_Vector;
                     New_Item : Storage_Array_Access);

   function Last_Element
     (Container : Storage_Vector) return Storage_Array_Access;

   procedure Delete_Last (Container : in out Storage_Vector);

   function Length (Container : Storage_Vector) return Natural;

   --  NOTE: Ada 95 allows multiple subprograms to be mentioned in a single
   --  Inline pragma, but Janus currently doesn't support this, which is why
   --  they are listed separately
   --
   pragma Inline (Append);
   pragma Inline (Last_Element);
   pragma Inline (Delete_Last);
   pragma Inline (Length);

   --------------------------------------------------------------

   procedure Allocate
     (Pool : in out Basic_Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      Next_Address : System.Address :=
        Pool.Active (Pool.Next_Allocation)'Address;

      use type Storage_Elements.Storage_Count;

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
        Pool.Active'Length - Pool.Next_Allocation;

   begin --  Allocate

      pragma Assert (Is_Owner (Pool, Current_Task));

      --  If there's not enough space in the current hunk of memory
      if Size_In_Storage_Elements + Alignment_Offset > Remaining then

         Append (Container => Pool.Used_List,
                 New_Item => Pool.Active);

         if Length (Pool.Free_List) > 0 and then
           Last_Element (Pool.Free_List)'Length >= Size_In_Storage_Elements
         then
            Pool.Active := Last_Element (Pool.Free_List);
            Delete_Last (Pool.Free_List);
         else
            Pool.Active := new System.Storage_Elements.Storage_Array
              (1 .. Storage_Elements.Storage_Count'Max
                 (Size_In_Storage_Elements, Pool.Block_Size));
         end if;

         Pool.Next_Allocation := Pool.Active'First;
         Next_Address := Pool.Active (Pool.Next_Allocation)'Address;
         Alignment_Offset := Get_Alignment_Offset;
      end if;

      Storage_Address := Next_Address + Alignment_Offset;

      Pool.Next_Allocation :=
        Pool.Next_Allocation + Size_In_Storage_Elements + Alignment_Offset;
   end Allocate;

   --------------------------------------------------------------

   procedure Append (Container : in out Storage_Vector;
                     New_Item : Storage_Array_Access)
   is
      use type System.Storage_Elements.Storage_Count;
   begin
      if Container.Last = Container.Storage.all'Length then
         declare
            Old : Vector_Access := Container.Storage;
         begin
            Container.Storage := new Vector (1 .. Old'Length * 2);
            Container.Storage.all (1 .. Old'Length) := Old.all;
            Free_Vector (Old);
         end;
      end if;

      Container.Last := Container.Last + 1;
      Container.Storage (Container.Last) := New_Item;
   end Append;

   --------------------------------------------------------------

   procedure Deallocate
     (Pool : in out Basic_Dynamic_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      null;
   end Deallocate;

   --------------------------------------------------------------

   procedure Delete_Last (Container : in out Storage_Vector) is
      use type System.Storage_Elements.Storage_Count;
   begin
      Container.Last := Container.Last - 1;
   end Delete_Last;

   --------------------------------------------------------------

   procedure Finalize   (Pool : in out Basic_Dynamic_Pool) is
   begin
      for I in 1 .. Pool.Used_List.Last loop
         Free_Storage_Array (Pool.Used_List.Storage (I));
      end loop;

      Free_Vector (Pool.Used_List.Storage);

      for I in 1 .. Pool.Free_List.Last loop
         Free_Storage_Array (Pool.Free_List.Storage (I));
      end loop;

      Free_Vector (Pool.Free_List.Storage);

      Free_Storage_Array (Pool.Active);
   end Finalize;

   --------------------------------------------------------------

   procedure Initialize (Pool : in out Basic_Dynamic_Pool) is
   begin
      Pool.Active := new System.Storage_Elements.Storage_Array
        (1 .. Pool.Block_Size);
      Pool.Next_Allocation := 1;
      Pool.Owner := Ada.Task_Identification.Current_Task;
      Pool.Used_List := Storage_Vector'
        (Storage => new Vector (1 .. 1024),
         Last => 1);
      Pool.Free_List := Storage_Vector'
        (Storage => new  Vector (1 .. 1024),
         Last => 1);
   end Initialize;

   --------------------------------------------------------------

   function Is_Owner
     (Pool : Basic_Dynamic_Pool;
      T : Task_Id := Current_Task) return Boolean is
   begin
      return (Pool.Owner = T);
   end Is_Owner;

   --------------------------------------------------------------

   function Last_Element (Container : Storage_Vector)
                           return Storage_Array_Access is
   begin
      return Container.Storage (Container.Last);
   end Last_Element;

   --------------------------------------------------------------

   function Length (Container : Storage_Vector) return Natural is
   begin
      return Natural (Container.Last);
   end Length;

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

   function Storage_Size
     (Pool : Basic_Dynamic_Pool)
      return Storage_Elements.Storage_Count
   is
      Result : Storage_Elements.Storage_Count := 0;

      use type Storage_Elements.Storage_Count;
   begin
      for I in 1 .. Pool.Used_List.Last loop
         Result := Result + Pool.Used_List.Storage.all'Length;
      end loop;
      for I in 1 .. Pool.Free_List.Last loop
         Result := Result + Pool.Free_List.Storage.all'Length;
      end loop;

      return Result + Pool.Active'Length;
   end Storage_Size;

   --------------------------------------------------------------

   function Storage_Used
     (Pool : Basic_Dynamic_Pool)
      return Storage_Elements.Storage_Count
   is
      Result : Storage_Elements.Storage_Count := 0;

      use type Storage_Elements.Storage_Count;
   begin
      for I in 1 .. Pool.Used_List.Last loop
         Result := Result + Pool.Used_List.Storage.all'Length;
      end loop;

      return Result + Pool.Next_Allocation - 1;
   end Storage_Used;

end Basic_Dynamic_Pools;
