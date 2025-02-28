------------------------------------------------------------------------------
--                                                                          --
--     Deepend - Dynamic Storage Pools for Ada 95, 2005, 2012, and 2022     --
--                                                                          --
--              T E S T _ D Y N A M I C _ P O O L S _ A D A 9 5             --
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
with Dynamic_Pools; use Dynamic_Pools;
with Ada.Text_IO; use Ada.Text_IO;
with System.Storage_Elements; use System;
with System.Address_Image;

procedure Test_Dynamic_Pools_Ada95
is

   Pool : aliased Dynamic_Pools.Dynamic_Pool
     (Default_Block_Size => Dynamic_Pools.Default_Allocation_Block_Size);

   type Id_String is new String (1 .. 10);
   pragma Pack (Id_String);

   type Id_String_Access is access all Id_String;
   for Id_String_Access'Storage_Pool use Pool;

   type String_Access is access all String;
   for String_Access'Storage_Pool use Pool;

   type Node_Type;
   type Node_Access is access all Node_Type;
   for Node_Access'Storage_Pool use Pool;

   type Node_Type is record
      Value : Integer;
      Name : String_Access;
      Description : Id_String_Access;
      Next : Node_Access;
   end record;

   package Node_Allocators is new Dynamic_Pools.Subpool_Allocators
     (Node_Type,
      Node_Access,
      Node_Type'(Value => 0, Name => null, Description => null, Next => null));

   Ordinary_Alignment : constant := 4;
   type Ordinary_Type is range 0 .. 2**16 - 1;
   for Ordinary_Type'Alignment use Ordinary_Alignment;
   for Ordinary_Type'Size use 16;

   type O_Access is access all Ordinary_Type;
   for O_Access'Storage_Pool use Pool;

   package Allocators is new Dynamic_Pools.Subpool_Allocators
     (Ordinary_Type,
      O_Access,
      Ordinary_Type'(0));

   package Id_String_Allocators is new Dynamic_Pools.Subpool_Allocators
     (Allocation_Type        => Id_String,
      Allocation_Type_Access => Id_String_Access,
      Default_Value          => Id_String'(others => ' '));

   function Recurse (Depth : Natural) return Node_Access
   is
      Sub_Pool : constant Dynamic_Pools.Subpool_Handle
        := Dynamic_Pools.Create_Subpool (Pool'Access);

      Node : constant Node_Access := Node_Allocators.Allocate (Sub_Pool);

      Name : constant String_Access :=
        new String'("Depth=" & Natural'Image (Depth));

      Description : constant Id_String_Access
        := Id_String_Allocators.Allocate (Subpool => Sub_Pool,
                                          Value   => "ABCDEFGHIJ");

   begin
      if Depth = 0 then
         Node.all := (Value => 0,
                      Name => Name.all'Unchecked_Access,
                      Description => Description,
                      Next => null);
         return  Node;
      else
         Node.all := (Value => Depth,
                      Name => Name.all'Unchecked_Access,
                      Description => Description,
                      Next => Recurse (Depth - 1));
         return Node;
      end if;
   end Recurse;

   procedure Print (List : Node_Type)
   is
   begin
      if List.Next /= null then
         Print (List.Next.all);
      end if;

      Put_Line (Integer'Image (List.Value) &
                ", Name=<" & List.Name.all &
                ">, Desc=<" & String (List.Description.all) & '>');
   end Print;

   procedure Deallocate_Default_Subpool
   is
      Default_Subpool : Dynamic_Pools.Subpool_Handle :=
        Default_Subpool_For_Pool (Pool'Access);
   begin

      Put_Line ("Deallocating Default Subpool");

      pragma Warnings (Off, "*Default_Subpool*modified*but*n* referenced*");

      Dynamic_Pools.Unchecked_Deallocate_Subpool (Default_Subpool);

      pragma Warnings (On, "*Default_Subpool*modified*but*n* referenced*");

      Put_Line ("Storage Used=" &
                  Storage_Elements.Storage_Count'Image (Storage_Used (Pool)) &
                  ", Storage Size=" &
                Storage_Elements.Storage_Count'Image (Storage_Size (Pool)));
   end Deallocate_Default_Subpool;

   List : Node_Access;
   Recursion_Depth : constant := 10;

   use type System.Storage_Elements.Storage_Offset;

begin --  Test_Dynamic_Pools_Ada95

   New_Line;
   Put_Line ("Initial Storage Used=" &
               Storage_Elements.Storage_Count'Image (Storage_Used (Pool)) &
               ", Storage Size=" &
               Storage_Elements.Storage_Count'Image (Storage_Size (Pool)));

   Put_Line ("Allocating List Recursively to" &
               Natural'Image (Recursion_Depth) & " subpools");
   Put_Line ("Note: List Nodes are node descriptions allocated to new");
   Put_Line ("       subpools, however, the node names in each node are");
   Put_Line ("       allocated to the default subpool");

   List := Recurse (Recursion_Depth);

   Put_Line ("Storage Used=" &
               Storage_Elements.Storage_Count'Image (Storage_Used (Pool)) &
               ", Storage Size=" &
               Storage_Elements.Storage_Count'Image (Storage_Size (Pool)));

   Put_Line ("Storage Used in Default Subpool=" &
               Storage_Elements.Storage_Count'Image
       (Dynamic_Pools.Storage_Used
          (Subpool => Default_Subpool_For_Pool (Pool'Access))) &
               ", Storage Size=" &
        Storage_Elements.Storage_Count'Image
        (Dynamic_Pools.Storage_Size
           (Subpool => Default_Subpool_For_Pool (Pool'Access))));

   Put_Line
     ("Bytes Stored in Other subpools=" &
        Storage_Elements.Storage_Count'Image
        (Storage_Used (Pool) - Dynamic_Pools.Storage_Used
           (Subpool => Default_Subpool_For_Pool (Pool'Access))));

   Nested_Subpool_Tests :
   begin

      Nested_Normal_Subpool_Test :
      declare
         Sub_Pool : Dynamic_Pools.Subpool_Handle
           := Dynamic_Pools.Create_Subpool (Pool'Access);
      begin

         Put_Line ("Allocating objects to a new subpool");

         for I in 1 .. 10 loop
            declare
               Object : constant O_Access := Allocators.Allocate (Sub_Pool);
               pragma Unreferenced (Object);
            begin
               null;
            end;
         end loop;

         Put_Line ("Bytes Stored=" &
                     Storage_Elements.Storage_Count'Image
                     (Storage_Used (Pool)));

         Put_Line ("Deallocating Subpool...");

         pragma Warnings (Off, "*Sub_Pool* modified*but*n* referenced*");
         Dynamic_Pools.Unchecked_Deallocate_Subpool (Sub_Pool);
         pragma Warnings (On, "*Sub_Pool* modified*but*n* referenced*");

      end Nested_Normal_Subpool_Test;

      Nested_Scoped_Subpool_Test :
      declare
         Sub_Pool : Dynamic_Pools.Scoped_Subpool
           (Pool => Pool'Access,
            Block_Size => Dynamic_Pools.Default_Allocation_Block_Size);
      begin

         Put_Line ("Allocating objects to a new scoped subpool");

         for I in 1 .. 10 loop
            declare
               Object : constant O_Access :=
                 Allocators.Allocate (Handle (Sub_Pool));
               pragma Unreferenced (Object);
            begin
               null;
            end;
         end loop;

         Put_Line ("Bytes Stored Before Finalization=" &
                     Storage_Elements.Storage_Count'Image
                     (Storage_Used (Pool)));

      end Nested_Scoped_Subpool_Test;

      Put_Line ("Bytes Stored After Finalization=" &
                  Storage_Elements.Storage_Count'Image
                  (Storage_Used (Pool)));

   end Nested_Subpool_Tests;

   Print (List.all);

   Deallocate_Default_Subpool;

   --  Reinstate a default subpool
   Dynamic_Pools.Create_Default_Subpool (Pool);

   Put_Line
     ("Bytes Stored in Default Subpool=" &
        Storage_Elements.Storage_Count'Image
        (Dynamic_Pools.Storage_Used
           (Subpool => Default_Subpool_For_Pool (Pool'Access))));

   declare
      Object : O_Access;
      Id     : Id_String_Access;
   begin
      Put_Line ("Allocating some more objects to the default subpool");
      Put_Line ("Allocating an object with an odd number of bytes");

      Id := new Id_String'("123456789A"); -- Allocate object of odd length
      Put_Line ("Id'Address=" & System.Address_Image (Id.all'Address));

      --  Check that we can allocate objects that require word alignment
      --  after having allocated something with an odd length

      Put_Line ("Now allocating objects that require word alignment");

      for I in 1 .. 10 loop
         Object := new Ordinary_Type'(Ordinary_Type (I));

         Put_Line ("Object'Address=" &
                     System.Address_Image (Object.all'Address));
         pragma Assert (Object.all'Address mod Ordinary_Alignment = 0
                        and then Object.all = Ordinary_Type (I));
      end loop;

      Put_Line ("Bytes Stored=" &
                  Storage_Elements.Storage_Count'Image (Storage_Used (Pool)));

      Put_Line
        ("Bytes Stored in Default Subpool=" &
           Storage_Elements.Storage_Count'Image
           (Dynamic_Pools.Storage_Size
              (Subpool => Default_Subpool_For_Pool (Pool'Access))));
   end;

   Put_Line ("Deallocating Default Subpool again");

   Deallocate_Default_Subpool;

   --  Reinstate a default subpool
   Dynamic_Pools.Create_Default_Subpool (Pool);

   Put_Line
     ("Bytes Stored in Default Subpool=" &
        Storage_Elements.Storage_Count'Image
        (Dynamic_Pools.Storage_Used
           (Subpool => Default_Subpool_For_Pool (Pool'Access))));
   Put_Line ("At this point, the nodes and their descriptions still exist,");
   Put_Line ("because their subpools still exist, however the node names");
   Put_Line ("shouldn't exist, because the default subpool had been freed.");
--     Put_Line ("In GNAT this still may print out, probably because the ");
--     Put_Line ("memory has not been overwritten, but this is dangerous");
--     Put_Line ("Don't be surprised if you see a storage error exception");
--
--     begin
--        Print (List.all);
--
--     exception
--        when Storage_Error =>
--           New_Line;
--           Put_Line ("STORAGE_ERROR was raised, which is not surprising");
--     end;

   New_Line;
   Put_Line ("Successful Completion");
   New_Line;

end Test_Dynamic_Pools_Ada95;
