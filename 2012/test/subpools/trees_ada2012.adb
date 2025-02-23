--  The Computer Language Benchmarks Game
--  http://shootout.alioth.debian.org/
--
--  Based on Ada versions created by
--    Jim Rogers and Brian Drummond as well as the
--    C version by Francesco Abbate
--
--  Contributed by Brad Moore

package body Trees_Ada2012 is

   function Create
     (Subpool : Subpool_Handle;
      Item : Integer;
      Depth : Integer) return Tree_Node
   is
      function Recurse (Item : Integer;
                        Depth : Integer) return Tree_Node is
        (new (Subpool) Node'
             (Left  => (if Depth > 0 then Recurse (2 * Item - 1, Depth - 1)
                        else null),
              Right => (if Depth > 0 then Recurse (2 * Item, Depth - 1)
                        else null),
              Value => Item));
   begin
      return Recurse (Item, Depth);
   end Create;

end Trees_Ada2012;
