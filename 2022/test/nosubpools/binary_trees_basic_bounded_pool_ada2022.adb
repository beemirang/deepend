--  The Computer Language Benchmarks Game
--  http://shootout.alioth.debian.org/
--
--  Based on Ada versions created by
--    Jim Rogers and Brian Drummond as well as the
--    C version by Francesco Abbate
--
--  Contributed by Brad Moore
--
--  Note: This version of the code uses different access types of differing
--  scopes for each pool thus relying on the access type finalization rather
--  than Deepends ability to allocate from different pool objects using the
--  same access type. In other words, all allocations are via the "new"
--  operator rather than through deepends "allocate" generics.

--  The requirements of the benchmark are;
--
--    * define a tree node class and methods, a tree node record and
--      procedures, or an algegraic data type and functions
--    * allocate a binary tree to 'stretch' memory, check it exists,
--      and deallocate it
--    * allocate a long-lived binary tree which will live-on while
--      other trees are allocated and deallocated
--    * allocate, walk, and deallocate many bottom-up binary trees
--          - allocate a tree
--          - walk the tree nodes, checksum the node items (and maybe
--            deallocate the node)
--          - deallocate the tree
--    * check that the long-lived binary tree still exists
--
--  Note: this is an adaptation of a benchmark for testing GC so we are
--     interested in the whole tree being allocated before any nodes are GC'd
--     - which probably excludes lazy evaluation.
--
--  Note: the left subtrees are heads of the right subtrees, keeping a depth
--     counter in the accessors to avoid duplication is cheating!
--
--  Note: the tree should have tree-nodes all the way down, replacing the
--  bottom nodes by some other value is not acceptable; and the bottom nodes
--  should be at depth 0.
--
--  Note: these programs are being measured with the default initial heap size
--     - the measurements may be very different with a larger initial heap
--       size or GC tuning.
--
--  The binary-trees benchmark is a simplistic adaptation of Hans Boehm's
--  GCBench, which in turn was adapted from a benchmark by John Ellis and
--  Pete Kovac.

pragma Restrictions
  (No_Implementation_Aspect_Specifications,
   No_Implementation_Attributes,
   No_Implementation_Identifiers,
   No_Implementation_Units);

with Trees.Creation;

with Basic_Bounded_Dynamic_Pools; use Basic_Bounded_Dynamic_Pools;

with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Task_Identification; use Ada.Task_Identification;

with System.Storage_Elements; use System.Storage_Elements;
with System.Multiprocessors;

procedure Binary_Trees_Basic_Bounded_Pool_Ada2022 is

   Default_Depth : constant := 20;
   Min_Depth     : constant := 4;

   Requested_Depth : constant Positive :=
     (if Argument_Count > 0 then Positive'Value (Argument (1))
      else Default_Depth);

   Max_Depth     : constant Positive :=
     Positive'Max (Min_Depth + 2, Requested_Depth);

   Depth_Iterations : constant Positive := (Max_Depth - Min_Depth) / 2 + 1;

   Worker_Count     : constant Positive :=
     (if Argument_Count > 1 then Positive'Value (Argument (2))
      else
         Positive'Min
           (Depth_Iterations,
         Positive (System.Multiprocessors.Number_Of_CPUs) +
             (Depth_Iterations mod Positive
              (System.Multiprocessors.Number_Of_CPUs))));

   task type Depth_Worker
     (Start, Finish : Positive := Positive'Last) is
   end Depth_Worker;

   Results : array (1 .. Depth_Iterations) of Integer;
   Iteration_Tracking : array (1 .. Depth_Iterations) of Positive;

   Failure_Detected : Boolean := False;
   pragma Atomic (Failure_Detected);

   task body Depth_Worker
   is
      Depth         : Natural;
      Check         : Integer;
      Iterations    : Positive;
   begin -- Depth_Worker

      for Depth_Iter in Start .. Finish loop

         Depth := Min_Depth + (Depth_Iter - 1) * 2;
         Iterations := 2 ** (Max_Depth - Depth + Min_Depth);
         Iteration_Tracking (Depth_Iter) := Iterations;

         Check      := 0;

         for I in 1 .. Iterations loop
            declare

               Short_Lived_Pool : Basic_Dynamic_Pool
                 (Size =>
                    2 * (2 ** (Depth + 1)) * Trees.Node_Size,
                  Heap_Allocated => True);
               --  Since we know how much storage we need, we might as well
               --  specify a block size large enough to hold all the objects
               --  in a single block

               type Short_Lived_Tree_Node is access Trees.Tree_Node
                 with Storage_Pool => Short_Lived_Pool;

               package Short_Lived_Tree_Creator is new Trees.Creation
                 (Short_Lived_Tree_Node);

               Short_Lived_Tree_1, Short_Lived_Tree_2 : Short_Lived_Tree_Node;
            begin

               Short_Lived_Tree_1 :=
                 Short_Lived_Tree_Creator.Create
                   (Item  => I,
                    Depth => Depth);

               Short_Lived_Tree_2 :=
                  Short_Lived_Tree_Creator.Create
                    (Item  => -I,
                     Depth => Depth);

               Check := Check +
                 Trees.Item_Check (Short_Lived_Tree_1) +
                 Trees.Item_Check (Short_Lived_Tree_2);

            end;
         end loop;

         Results (Depth_Iter) := Check;
      end loop;

   exception
      when E : others =>
         Failure_Detected := True;
         Put_Line ("Depth Worker Failed: " & Exception_Information (E));

   end Depth_Worker;

   subtype Worker_Id is Positive range 1 .. Worker_Count;

   Start_Index         : Positive := 1;
   End_Index           : Positive := Depth_Iterations;

   Iterations_Per_Task : constant Positive := Depth_Iterations / Worker_Count;
   Remainder           : Natural := Depth_Iterations rem Worker_Count;

   function Create_Worker return Depth_Worker is
   begin
      if Remainder = 0 then
         End_Index := Start_Index + Iterations_Per_Task - 1;
      else
         End_Index := Start_Index + Iterations_Per_Task;
         Remainder := Remainder - 1;
      end if;

      return New_Worker : Depth_Worker
        (Start => Start_Index,
         Finish => End_Index)
      do
         Start_Index := End_Index + 1;
      end return;
   end Create_Worker;

   Long_Lived_Tree_Pool : Basic_Dynamic_Pool
     (Size => 2 ** (Max_Depth + 1) * Trees.Node_Size,
      Heap_Allocated => True);
   --  Since we know how much storage we need, we might as well
   --  specify a block size large enough to hold all the objects
   --  in a single block

   type Long_Lived_Tree_Node is access Trees.Tree_Node
     with Storage_Pool => Long_Lived_Tree_Pool;

   package Long_Lived_Tree_Creator is new
     Trees.Creation (Long_Lived_Tree_Node);

   Long_Lived_Tree : Long_Lived_Tree_Node;

   Check : Integer;

begin --  Binary_Trees_Basic_Bounded_Pool_Ada2022

   --  The main task relinquishes ownership of the default subpool.
   Set_Owner (Pool => Long_Lived_Tree_Pool,
              T => Null_Task_Id);

   --  Do the stretch tree processing at the same time that the long lived
   --  tree is being created.
   declare
      task Stretch_Depth_Task;

      task body Stretch_Depth_Task is

         Stretch_Depth : constant Positive := Max_Depth + 1;

         Stretch_Pool : Basic_Dynamic_Pool
           (Size => 2 ** (Stretch_Depth + 1) * Trees.Node_Size,
            Heap_Allocated => True);
         --  Since we know how much storage we need, we might as well
         --  specify a block size large enough to hold all the objects
         --  in a single block

         type Stretch_Node is access Trees.Tree_Node;
         for Stretch_Node'Storage_Pool use Stretch_Pool;

         package Stretch_Tree_Creator is new Trees.Creation (Stretch_Node);

         Stretch_Tree : constant Stretch_Node :=
           Stretch_Tree_Creator.Create (Item  => 0,
                                        Depth => Stretch_Depth);
      begin --  Stretch_Depth_Task
         Check        := Trees.Item_Check (Stretch_Tree);
         Put ("stretch tree of depth ");
         Put (Item => Stretch_Depth, Width => 1);
         Put (HT & " check: ");
         Put (Item => Check, Width => 1);
         New_Line;

      exception
         when E : others =>
            Failure_Detected := True;
            Put_Line
              ("Stretch Depth Task Failed: " & Exception_Information (E));
      end Stretch_Depth_Task;

      task Create_Long_Lived_Tree_Task is
      end Create_Long_Lived_Tree_Task;

      task body Create_Long_Lived_Tree_Task is
      begin
         --  Since the main task relinquished ownership, we can take ownership
         --  here.
         Set_Owner (Long_Lived_Tree_Pool);
         Long_Lived_Tree := Long_Lived_Tree_Creator.Create (0, Max_Depth);

      exception
         when E : others =>
            Failure_Detected := True;
            Put_Line ("Long Lived Task Failed: " & Exception_Information (E));
      end Create_Long_Lived_Tree_Task;
   begin
      null;
   end;

   --  Now process the trees of different sizes in parallel and collect results
   declare
      Workers : array (Worker_Id) of Depth_Worker := [others => Create_Worker];
      pragma Unreferenced (Workers);
   begin
      null;
   end;

   --  Now output the results
   for I in Results'Range loop
      Put (Item => Iteration_Tracking (I) * 2, Width => 0);
      Put (HT & " trees of depth ");
      Put (Item => Min_Depth + 2 * (I - 1), Width => 0);
      Put (HT & " check: ");
      Put (Item => Results (I), Width => 0);
      New_Line;
   end loop;

   --  Verify that the long lived tree still exists
   Put ("long lived tree of depth ");
   Put (Item => Max_Depth, Width => 0);
   Put (HT & " check: ");
   Check := Trees.Item_Check (Long_Lived_Tree);
   Put (Item => Check, Width => 0);
   New_Line;

   if Failure_Detected then
      New_Line;
      Put_Line ("ERROR: Some Tasks failed");
      New_Line;
   end if;

end Binary_Trees_Basic_Bounded_Pool_Ada2022;
