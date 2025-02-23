--  The Computer Language Benchmarks Game
--  http://shootout.alioth.debian.org/
--
--  Based on Ada versions created by
--    Jim Rogers and Brian Drummond as well as the
--    C version by Francesco Abbate
--
--  Contributed by Brad Moore

generic
   type Tree_Node_Access is access Tree_Node;
package Trees.Creation is

   function Create
     (Item : Integer;
      Depth : Integer) return Tree_Node_Access;

private

   function Create (Item : Integer;
                    Depth : Integer) return Tree_Node_Access is
     (new Tree_Node'
        (Left  => (if Depth <= 0 then null
                   else Create (2 * Item - 1, Depth - 1).all'Unchecked_Access),
         Right => (if Depth <= 0 then null
                   else Create (2 * Item, Depth - 1).all'Unchecked_Access),
         Value => Item));

end Trees.Creation;
