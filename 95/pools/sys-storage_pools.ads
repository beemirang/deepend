------------------------------------------------------------------------------
--                                                                          --
--     Deepend - Dynamic Storage Pools for Ada 95, 2005, 2012, and 2022     --
--                                                                          --
--                   S Y S . S T O R A G E   P O O L S                      --
--                                                                          --
--                                S p e c                                   --
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
--  Dummy package intended to represent the standard System.Storage_Pools
--  package.
--  This makes it easier to make it appear that a child of System.Storage_Pools
--  is being used, since you cant declare your own child packages
--  under System. In the Ada 2012 version, this package no longer exists, as
--  System.Storage_Pools has all the needed child packages.
package Sys.Storage_Pools is
   pragma Preelaborate;
end Sys.Storage_Pools;
