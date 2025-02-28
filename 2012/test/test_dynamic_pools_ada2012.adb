------------------------------------------------------------------------------
--                                                                          --
--     Deepend - Dynamic Storage Pools for Ada 95, 2005, 2012, and 2022     --
--                                                                          --
--            T E S T _ D Y N A M I C _ P O O L S _ A D A 2 0 1 2           --
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
pragma Restrictions
  (No_Implementation_Aspect_Specifications,
   No_Implementation_Attributes,
   No_Implementation_Identifiers);

with Dynamic_Pools; use Dynamic_Pools;
with Ada.Text_IO; use Ada.Text_IO;
with System.Storage_Elements; use System;
with System.Address_Image;
with Ada.Finalization;

procedure Test_Dynamic_Pools_Ada2012
is

   Pool : Dynamic_Pools.Dynamic_Pool;
   pragma Default_Storage_Pool (Pool);

   type Id_String is new String (1 .. 10);
   pragma Pack (Id_String);
   type Id_String_Access is access Id_String with Storage_Pool => Pool;

   type String_Access is access String with Storage_Pool => Pool;

   type Node_Type is record
      Value : Integer;
      Name : access String;
      Description : Id_String_Access;
      Next : access Node_Type;
   end record;

   type Node_Access is access Node_Type with Storage_Pool => Pool;

   Ordinary_Alignment : constant := 4;
   type Ordinary_Type is range 0 .. 2**16 - 1 with
      Alignment => Ordinary_Alignment,
      Size      => 16;

   type Reference_Counted_Type is new Ada.Finalization.Controlled with
      record
         Value : Integer;
      end record;

   Object_Count : Natural := 0;

   overriding procedure Initialize
     (Object : in out Reference_Counted_Type);
   overriding procedure Finalize
     (Object : in out Reference_Counted_Type);
   overriding procedure Adjust
     (Object : in out Reference_Counted_Type);

   overriding procedure Initialize (Object : in out Reference_Counted_Type)
   is
   begin
      Object.Value := 0;
      Object_Count := Object_Count + 1;
      Put_Line ("Called Init");
   end Initialize;

   overriding procedure Adjust (Object : in out Reference_Counted_Type)
   is
   begin
      Object.Value := 0;
      Object_Count := Object_Count + 1;
      Put_Line ("Called Adjust");
   end Adjust;

   overriding procedure Finalize   (Object : in out Reference_Counted_Type)
   is
   begin
      Object.Value := -1;
      Object_Count := Object_Count - 1;
      Put_Line ("Called Final");
   end Finalize;

   function Recurse (Depth : Natural) return Node_Access
   is
      Sub_Pool : constant Dynamic_Pools.Subpool_Handle
        := Dynamic_Pools.Create_Subpool (Pool);

      Node : constant Node_Access := new (Sub_Pool) Node_Type;

      Name : constant String_Access :=
        new String'("Depth=" & Natural'Image (Depth));

      Description : constant Id_String_Access
        := new (Sub_Pool) Id_String'("ABCDEFGHIJ");

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
        Pool.Default_Subpool_For_Pool;
   begin

      Put_Line ("Deallocating Default Subpool");

      pragma Warnings (Off, "*Default_Subpool*modified*but*n* referenced*");

      Dynamic_Pools.Unchecked_Deallocate_Subpool (Default_Subpool);

      pragma Warnings (On, "*Default_Subpool*modified*but*n* referenced*");

      Put_Line ("Storage Used=" &
                  Storage_Elements.Storage_Count'Image (Pool.Storage_Used) &
                  ", Storage Size=" &
                  Storage_Elements.Storage_Count'Image (Pool.Storage_Size));
   end Deallocate_Default_Subpool;

   List : Node_Access;

   type RC_Access is access Reference_Counted_Type with Storage_Pool => Pool;

   type O_Access is access Ordinary_Type with Storage_Pool => Pool;

   Recursion_Depth : constant := 10;

   use type System.Storage_Elements.Storage_Offset;

begin --  Test_Dynamic_Pools_Ada2012

   New_Line;
   Put_Line ("Initial Storage Used=" &
               Storage_Elements.Storage_Count'Image (Pool.Storage_Used) &
               ", Storage Size=" &
               Storage_Elements.Storage_Count'Image (Pool.Storage_Size));

   Put_Line ("Allocating List Recursively to" &
               Natural'Image (Recursion_Depth) & " subpools");
   Put_Line ("Note: List Nodes are node descriptions allocated to new");
   Put_Line ("       subpools, however, the node names in each node are");
   Put_Line ("       allocated to the default subpool");

   List := Recurse (Recursion_Depth);

   declare
      Total_Storage_Used : constant Storage_Elements.Storage_Count :=
        Pool.Storage_Used;

      Subpool_Storage_Used : constant Storage_Elements.Storage_Count :=
        Dynamic_Pools.Storage_Used
             (Subpool => Pool.Default_Subpool_For_Pool);
   begin

      Put_Line ("Storage Used=" &
                  Storage_Elements.Storage_Count'Image (Total_Storage_Used) &
                  ", Storage Size=" &
                  Storage_Elements.Storage_Count'Image (Pool.Storage_Size));

      Put_Line ("Storage Used in Default Subpool=" &
                  Storage_Elements.Storage_Count'Image (Subpool_Storage_Used) &
                  ", Storage Size=" &
          Storage_Elements.Storage_Count'Image
          (Dynamic_Pools.Storage_Size
             (Subpool => Pool.Default_Subpool_For_Pool)));

      Put_Line
        ("Bytes Stored in Other subpools=" &
           Storage_Elements.Storage_Count'Image
           (Total_Storage_Used - Subpool_Storage_Used));
   end;

   pragma Warnings (Off, "*Object*assigned but never read*");

   declare
      Sub_Pool : Dynamic_Pools.Subpool_Handle
        := Dynamic_Pools.Create_Subpool (Pool);

      Object : RC_Access;
   begin

      Put_Line ("Allocating controlled objects needing finalization " &
                  "to a new subpool");

      for I in 1 .. 10 loop
         Object := new (Sub_Pool)
           Reference_Counted_Type;
      end loop;

      Put_Line ("Object Count=" & Natural'Image (Object_Count));
      Put_Line ("Bytes Stored=" &
                  Storage_Elements.Storage_Count'Image
                  (Pool.Storage_Used));

      Put_Line ("Deallocating Subpool...");

      pragma Warnings (Off, "*Sub_Pool* modified* but* n* referenced*");
      Dynamic_Pools.Unchecked_Deallocate_Subpool (Sub_Pool);
      pragma Warnings (On, "*Sub_Pool* modified* but* n* referenced*");

      Put_Line ("Object Count=" & Natural'Image (Object_Count));
      Put_Line ("Bytes Stored=" &
                  Storage_Elements.Storage_Count'Image (Pool.Storage_Used));
   end;

   declare

      pragma Suppress (Accessibility_Check);

      Sub_Pool : constant Dynamic_Pools.Scoped_Subpool
        := Dynamic_Pools.Create_Subpool (Pool => Pool);

      pragma Unsuppress (Accessibility_Check);

      Object : RC_Access;
   begin

      Put_Line ("Allocating controlled objects needing finalization " &
                  "to a subpool declared on the stack");

      for I in 1 .. 10 loop
         Object := new (Sub_Pool.Handle) Reference_Counted_Type;
      end loop;

      Put_Line ("Object Count=" & Natural'Image (Object_Count));
      Put_Line ("Bytes Stored=" &
                  Storage_Elements.Storage_Count'Image (Pool.Storage_Used));

   end;

   pragma Warnings (On, "*Object*assigned but never read*");

   Put_Line ("After Finalization, Object Count=" &
               Natural'Image (Object_Count));
   Put_Line ("Bytes Stored=" &
                  Storage_Elements.Storage_Count'Image (Pool.Storage_Used));

   Nested_Subpool_Tests :
   begin

      Nested_Normal_Subpool_Test :
      declare
         Sub_Pool : Dynamic_Pools.Subpool_Handle
           := Dynamic_Pools.Create_Subpool (Pool);
      begin

         Put_Line ("Allocating objects to a new subpool");

         for I in 1 .. 10 loop
            declare
               Object : constant O_Access
                 := new (Sub_Pool) Ordinary_Type'(Ordinary_Type (I));
            begin
               Put_Line ("Object Value=" & Ordinary_Type'Image (Object.all));
            end;
         end loop;

         Put_Line ("Bytes Stored=" &
                     Storage_Elements.Storage_Count'Image (Pool.Storage_Used));

         Put_Line ("Deallocating Subpool...");

         pragma Warnings (Off, "*Sub_Pool* modified*but*n* referenced*");
         Dynamic_Pools.Unchecked_Deallocate_Subpool (Sub_Pool);
         pragma Warnings (On, "*Sub_Pool* modified*but*n* referenced*");

      end Nested_Normal_Subpool_Test;

      Nested_Scoped_Subpool_Test :
      declare
         pragma Suppress (Accessibility_Check);

         Sub_Pool : constant Dynamic_Pools.Scoped_Subpool
           := Dynamic_Pools.Create_Subpool (Pool);

         pragma Unsuppress (Accessibility_Check);
      begin

         Put_Line ("Allocating objects to a new scoped subpool");

         for I in 1 .. 10 loop
            declare
               Object : constant O_Access
                 := new (Sub_Pool.Handle) Ordinary_Type'(Ordinary_Type (I));

               pragma Unreferenced (Object);
            begin
               null;
            end;
         end loop;

         Put_Line ("Bytes Stored Before Finalization=" &
                     Storage_Elements.Storage_Count'Image (Pool.Storage_Used));

      end Nested_Scoped_Subpool_Test;

      Put_Line ("Bytes Stored After Finalization=" &
                  Storage_Elements.Storage_Count'Image (Pool.Storage_Used));

   end Nested_Subpool_Tests;

   Print (List.all);

   Deallocate_Default_Subpool;

   --  Reinstate a default subpool
   Pool.Create_Default_Subpool;

   Put_Line
     ("Bytes Stored in Default Subpool=" &
        Storage_Elements.Storage_Count'Image
        (Dynamic_Pools.Storage_Used
           (Subpool => Pool.Default_Subpool_For_Pool)));

   declare
      Object : RC_Access;
      Id     : Id_String_Access;
   begin
      Put_Line ("Allocating some more objects needing finalization " &
                  "to the default subpool");
      Put_Line ("Allocating an object with an odd number of bytes");

      Id := new Id_String'("123456789A"); -- Allocate object of odd length
      Put_Line ("Id'Address=" & System.Address_Image (Id.all'Address));

      --  Check that we can allocate objects that require word alignment
      --  after having allocated something with an odd length

      Put_Line ("Now allocating objects that require word alignment");

      for I in 1 .. 10 loop
         Object := new
           Reference_Counted_Type'
             (Ada.Finalization.Controlled with Value => I);

         --  In this loop we are allocating initialized objects, so
         --  Initialize does not get called to increment the object count.
         --  We have do that here, explicitly.
         Object_Count := Object_Count + 1;

         Put_Line ("Object'Address=" &
                     System.Address_Image (Object.all'Address));
         pragma Assert (Object.all'Address mod Ordinary_Alignment = 0
                        and then Object.all.Value = I);
      end loop;

      Put_Line ("Object Count=" & Natural'Image (Object_Count));
      Put_Line ("Bytes Stored=" &
                  Storage_Elements.Storage_Count'Image (Pool.Storage_Used));

      Put_Line
        ("Bytes Stored in Default Subpool=" &
           Storage_Elements.Storage_Count'Image
           (Dynamic_Pools.Storage_Used
              (Subpool => Pool.Default_Subpool_For_Pool)));
   end;

   Put_Line ("Deallocating Default Subpool again");

   Deallocate_Default_Subpool;

   --  Reinstate a default subpool
   Pool.Create_Default_Subpool;

   Put_Line
     ("Bytes Stored in Default Subpool=" &
        Storage_Elements.Storage_Count'Image
        (Dynamic_Pools.Storage_Used
           (Subpool => Pool.Default_Subpool_For_Pool)));

   Put_Line ("At this point, the nodes and their descriptions still exist,");
   Put_Line ("because their subpools still exist, however the node names");
   Put_Line ("shouldn't exist, because the default subpool had been freed.");
   Put_Line ("In GNAT this still may print out, probably because the memory");
   Put_Line ("has not been overwritten, but this is dangerous");
   Put_Line ("Don't be surprised if you see a storage error exception");

   begin
      Print (List.all);
   exception
      when Storage_Error =>
         New_Line;
         Put_Line ("STORAGE_ERROR was raised, which is not surprising");
   end;

   New_Line;
   Put_Line ("Successful Completion");
   New_Line;

end Test_Dynamic_Pools_Ada2012;
