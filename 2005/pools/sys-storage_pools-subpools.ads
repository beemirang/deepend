------------------------------------------------------------------------------
--
--     Deepend - Dynamic Storage Pools for Ada 95, 2005, 2012, and 2022
--
--                        D Y N A M I C   P O O L S
--
--                                B o d y
--
--                  Copyright (C) 2011, Bradley J. Moore
--
--  Deepend is free software;  you can  redistribute it  and/or modify it
--  under  terms of the  GNU General Public License  as  published  by the
--  Free Software  Foundation;  either version 2,  or (at your option) any
--  later  version.  Deepend is  distributed in the hope that it  will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE.  See the GNU
--  General Public License for  more details.  You should have  received a
--  copy of the GNU General Public License distributed with Deepend;  see
--  file  COPYING.  If  not,  write  to  the  Free  Software  Foundation,
--  51 Franklin  Street,  Fifth  Floor, Boston, MA 02110-1301, USA.
--
--  As a  special exception, if other files  instantiate generics from
--  this unit,  or you link this  unit with other files  to produce an
--  executable,  this unit  does  not by  itself  cause the  resulting
--  executable to be covered by  the GNU General Public License.  This
--  exception does  not however invalidate  any other reasons  why the
--  executable file might be covered by the GNU Public License.
------------------------------------------------------------------------------
--  This is intended to closely map the Ada 2012 support for Storage
--  Subpools, and represents the package System.Storage_Pools.Subpools as
--  defined in AI05-0111-3. In the Ada 2012 version, this package no longer
--  exists as it is replaced by System.Storage_Pools.Subpools.

with System.Storage_Elements; use System.Storage_Elements;

--  BJM needed to make this package look like a child of System.Storage_Pools
with System.Storage_Pools; use System; use System.Storage_Pools;

package Sys.Storage_Pools.Subpools is
   pragma Preelaborate (Subpools);

   Ada_2012_Warnings : constant Boolean := False;

   type Root_Storage_Pool_With_Subpools is abstract new
     Root_Storage_Pool with private;
   --  An access type must have a storage pool of a type
   --  descended from this type to use subpools.

   type Root_Subpool is abstract tagged limited private;
   --  The subpool object type. Objects of this type are managed by
   --  the storage pool; usually only the handles are exposed
   --  to clients.

   type Subpool_Handle is access all Root_Subpool'Class;
   for Subpool_Handle'Storage_Size use 0;
   --  This provides a reference to a subpool, and serves to identify
   --  the subpool within the program.

   function Create_Subpool
     (Pool : not null access Root_Storage_Pool_With_Subpools)
      return not null Subpool_Handle is abstract;
   --  Create subpool within given storage pool.
   --  NOTE: Additional functions that create subpools may be
   --        defined for a given storage pool that supports subpools.

   function Pool_Of_Subpool
     (Subpool : not null Subpool_Handle)
      return access Root_Storage_Pool_With_Subpools'Class;
   --  Return access to underlying storage pool of given handle.

   procedure Set_Pool_Of_Subpool
     (Subpool : not null Subpool_Handle;
      To : in out Root_Storage_Pool_With_Subpools'Class);
   --  Set the Pool for a newly created subpool or a subpool that
   --  is being reused after a call to Unchecked_Deallocate_Subpool.
   --  This is intended to be called from Create_Subpool or similar
   --  subpool constructors.
   --  Raises Program_Error if the Pool already belongs to a pool.

   procedure Allocate_From_Subpool
     (Pool : in out Root_Storage_Pool_With_Subpools;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Subpool : not null Subpool_Handle) is abstract;
   --  with Pre'Class => Pool_of_Subpool(Subpool) = Pool'Access;
   --  Allocate space from specified subpool.

   procedure Deallocate_Subpool
     (Pool : in out Root_Storage_Pool_With_Subpools;
      Subpool : in out Subpool_Handle) is abstract;
   --  with Pre'Class => Pool_of_Subpool(Subpool) = Pool'Access;
   --  Deallocate the space for all of the objects allocated from the
   --  specified subpool. Unchecked_Deallocate_Subpool calls this.

   function Default_Subpool_For_Pool
     (Pool : not null access Root_Storage_Pool_With_Subpools)
      return not null Subpool_Handle;
   --  Returns a handle of the default subpool for Pool.
   --  This version raises Program_Error; it should be overridden for
   --  types that wish to support default subpools (that is, allocators
   --  without a subpool_specification).
   pragma Compile_Time_Warning
     (Ada_2012_Warnings, "Pool is an in out parameter in Ada 2012");

   overriding
   procedure Allocate
     (Pool : in out Root_Storage_Pool_With_Subpools;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);
   --  Normally not used.

   overriding
   procedure Deallocate
     (Pool : in out Root_Storage_Pool_With_Subpools;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is null;
   --  Normally, concrete types will inherit this null version.

   overriding
   function Storage_Size
     (Pool : Root_Storage_Pool_With_Subpools)
      return Storage_Count;
   --  is (Storage_Elements.Storage_Count'Last);
   --  This can be overridden if there is a limit.

private

   type Root_Storage_Pool_With_Subpools is abstract new
     Root_Storage_Pool with null record;

   type Root_Subpool is abstract tagged limited record
      Pool : access Root_Storage_Pool_With_Subpools'Class;
   end record;

   pragma Inline (Pool_Of_Subpool, Allocate, Set_Pool_Of_Subpool);
   pragma Inline (Storage_Size, Default_Subpool_For_Pool);
end Sys.Storage_Pools.Subpools;
