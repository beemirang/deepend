with Dynamic_Pools; use Dynamic_Pools;
with Ada.Text_IO; use Ada.Text_IO;
with System.Storage_Elements; use System;

procedure Test_Dynamic_Pools_Ada95
is

   Pool : aliased Dynamic_Pools.Dynamic_Pool
     (Default_Block_Size => Dynamic_Pools.Default_Allocation_Block_Size);

   subtype Id_String is String (1 .. 10);
   type Id_String_Access is access Id_String;
   for Id_String_Access'Storage_Pool use Pool;

   type String_Access is access all String;
   for String_Access'Storage_Pool use Pool;

   type Node_Type;
   type Node_Access is access Node_Type;
   for Node_Access'Storage_Pool use Pool;

   type Node_Type is record
      Value : Integer;
      Name : String_Access;
      Description : Id_String_Access;
      Next : Node_Access;
   end record;

   function New_Node is new Dynamic_Pools.Allocation
     (Node_Type,
      Node_Access);

   type Ordinary_Type is
      record
         Value : Integer;
      end record;

   type O_Access is access Ordinary_Type;
   for O_Access'Storage_Pool use Pool;

   function New_Ordinary_Type is new Dynamic_Pools.Allocation
     (Ordinary_Type,
      O_Access);

   function New_String is new Dynamic_Pools.Initialized_Allocation
     (Allocation_Type => Id_String,
      Allocation_Type_Access => Id_String_Access);

   function Recurse (Depth : Natural) return Node_Access
   is
      Sub_Pool : constant Dynamic_Pools.Subpool_Handle
        := Dynamic_Pools.Create_Subpool (Pool'Access);

      Node : constant Node_Access := New_Node (Sub_Pool);

      Name : constant String_Access :=
        new String'("Depth=" & Natural'Image (Depth));

      Description : constant Id_String_Access
        := New_String (Subpool => Sub_Pool,
                       Qualified_Expression => "ABCDEFGHIJ");

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
                ">, Desc=<" & List.Description.all & '>');
   end Print;

   procedure Deallocate_Default_Subpool
   is
      Default_Subpool : Dynamic_Pools.Subpool_Handle :=
        Default_Subpool_For_Pool (Pool);
   begin

      Put_Line ("Deallocating Default Subpool");

      pragma Warnings (Off, "*Default_Subpool*modified*but*never referenced*");

      Dynamic_Pools.Unchecked_Deallocate_Subpool (Default_Subpool);

      pragma Warnings (On, "*Default_Subpool*modified*but*never referenced*");

      Put_Line ("Bytes Stored=" &
                Storage_Elements.Storage_Count'Image (Storage_Size (Pool)));
   end Deallocate_Default_Subpool;

   List : Node_Access;
   Recursion_Depth : constant := 10;

   use type System.Storage_Elements.Storage_Offset;

begin

   New_Line;
   Put_Line ("Initial Bytes Stored=" &
               Storage_Elements.Storage_Count'Image (Storage_Size (Pool)));

   Put_Line ("Allocating List Recursively to" &
               Natural'Image (Recursion_Depth) & " subpools");
   Put_Line ("Note: List Nodes are node descriptions allocated to new");
   Put_Line ("       subpools, however, the node names in each node are");
   Put_Line ("       allocated to the default subpool");

   List := Recurse (Recursion_Depth);

   Put_Line ("Bytes Stored=" &
               Storage_Elements.Storage_Count'Image (Storage_Size (Pool)));

   Put_Line
     ("Bytes Stored in Default Subpool=" &
        Storage_Elements.Storage_Count'Image
        (Dynamic_Pools.Storage_Size
           (Subpool => Default_Subpool_For_Pool (Pool))));

   Put_Line
     ("Bytes Stored in Other subpools=" &
        Storage_Elements.Storage_Count'Image
        (Storage_Size (Pool) - Dynamic_Pools.Storage_Size
           (Subpool => Default_Subpool_For_Pool (Pool))));

   begin

      declare
         Sub_Pool : Dynamic_Pools.Subpool_Handle
           := Dynamic_Pools.Create_Subpool (Pool'Access);
      begin

         Put_Line ("Allocating objects to a new subpool");

         for I in 1 .. 10 loop
            declare
               Object : constant O_Access
                 := New_Ordinary_Type (Sub_Pool);
               pragma Unreferenced (Object);
            begin
               null;
            end;
         end loop;

         Put_Line ("Bytes Stored=" &
                     Storage_Elements.Storage_Count'Image
                     (Storage_Size (Pool)));

         Put_Line ("Deallocating Subpool...");

         pragma Warnings (Off, "*Sub_Pool* modified*but*never referenced*");
         Dynamic_Pools.Unchecked_Deallocate_Subpool (Sub_Pool);
         pragma Warnings (On, "*Sub_Pool* modified*but*never referenced*");
      end;

      Put_Line ("Bytes Stored=" &
                  Storage_Elements.Storage_Count'Image (Storage_Size (Pool)));
   end;

   Print (List.all);

   Deallocate_Default_Subpool;

   Put_Line
     ("Bytes Stored in Default Subpool=" &
        Storage_Elements.Storage_Count'Image
        (Dynamic_Pools.Storage_Size
           (Subpool => Default_Subpool_For_Pool (Pool))));

   pragma Warnings (Off, "*Object*is assigned but never read*");
   declare
      Object : O_Access;
   begin
      Put_Line ("Allocating some more objects to the default subpool");

      for I in 1 .. 10 loop
         Object := new Ordinary_Type;
      end loop;

      Put_Line ("Bytes Stored=" &
                  Storage_Elements.Storage_Count'Image (Storage_Size (Pool)));

      Put_Line
        ("Bytes Stored in Default Subpool=" &
           Storage_Elements.Storage_Count'Image
           (Dynamic_Pools.Storage_Size
              (Subpool => Default_Subpool_For_Pool (Pool))));
   end;
   pragma Warnings (On, "*Object*is assigned but never read*");

   Put_Line ("Deallocating Default Subpool again");

   Deallocate_Default_Subpool;

   Put_Line
     ("Bytes Stored in Default Subpool=" &
        Storage_Elements.Storage_Count'Image
        (Dynamic_Pools.Storage_Size
           (Subpool => Default_Subpool_For_Pool (Pool))));

   New_Line;
   Put_Line ("Successful Completion");
   New_Line;

end Test_Dynamic_Pools_Ada95;