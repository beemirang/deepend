------------------------------------------------------------------------------
--                                                                          --
--     Deepend - Dynamic Storage Pools for Ada 95, 2005, 2012, and 2022     --
--                                                                          --
--            S Y S . S T O R A G E _ P O O L S . S U B P O O L S           --
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
--  This is intended to closely map to the Ada 2012 Storage Subpools
--  package, and represents the package System.Storage_Pools.Subpools as
--  defined in AI05-0111-3. In the Ada 2012 version of Deepend, this package
--  does not exist, as it is replaced by System.Storage_Pools.Subpools.

package body Sys.Storage_Pools.Subpools is

   overriding
   procedure Allocate
     (Pool : in out Root_Storage_Pool_With_Subpools;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      null;
   end Allocate;

   function Default_Subpool_For_Pool
     (Pool : not null access Root_Storage_Pool_With_Subpools)
      return not null Subpool_Handle is
   begin
      raise Program_Error with "Default subpools not implemented in Ada 2005";
      return Default_Subpool_For_Pool (Pool);
   end Default_Subpool_For_Pool;

   function Pool_Of_Subpool
     (Subpool : not null Subpool_Handle)
      return access Root_Storage_Pool_With_Subpools'Class is
   begin
      return Subpool.Pool;
   end Pool_Of_Subpool;

   procedure Set_Pool_Of_Subpool
     (Subpool : not null Subpool_Handle;
      To : in out Root_Storage_Pool_With_Subpools'Class) is
   begin
      Subpool.Pool := To'Unchecked_Access;
   end Set_Pool_Of_Subpool;

   overriding
   function Storage_Size
     (Pool : Root_Storage_Pool_With_Subpools)
      return Storage_Count is
      pragma Unreferenced (Pool);
   begin
      return Storage_Count'Last;
   end Storage_Size;

end Sys.Storage_Pools.Subpools;
