--  SPDX-License-Identifier: MPL-2.0

--  Generic bounded ring buffer

--  TODO: i should not implement a ring buffer / queue and instead
--  should use an existing implementation... which exists, right?

with Ada.Iterator_Interfaces;

with Ring_Buffer_Core;

generic
   type Element is private;
   Element_Placeholder : Element;
package Ring_Buffer is
   package Core is new Ring_Buffer_Core (Element, Element_Placeholder);
   subtype Capacity_Type is Core.Capacity_Type;

   --  Buffer is an iterator
   type Cursor (Length : Natural) is record
      Index : Natural;
   end record;
   function Has_Element (Position : Cursor) return Boolean;

   package Buffer_Iterator_Interfaces is new
     Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Buffer (Max_Capacity : Capacity_Type) is
     new Buffer_Iterator_Interfaces.Reversible_Iterator with private
   with
     Iterator_Element => Element,
     Default_Iterator => Iterate,
     Constant_Indexing => Get;

   function Buffer_Init (Max_Capacity : Capacity_Type) return Buffer;

   --  Buffer operations

   function Length (B : Buffer) return Natural;
   function Is_Empty (B : Buffer) return Boolean;
   function Is_Full (B : Buffer) return Boolean;
   function Get (B : Buffer; I : Positive) return Element;
   procedure Push (B : in out Buffer; V : Element);
   function Pop (B : in out Buffer) return Element;
   procedure Clear (B : in out Buffer);
   procedure Truncate_Back (B : in out Buffer; Length : Natural);

   --  Iterable container interface

   function Iterate
     (Container : Buffer)
      return Buffer_Iterator_Interfaces.Reversible_Iterator'Class;

   function Get (Container : Buffer; Position : Cursor) return Element;

   overriding
   function First (Object : Buffer) return Cursor;

   overriding
   function Last (Object : Buffer) return Cursor;

   overriding
   function Next (Object : Buffer; Position : Cursor) return Cursor;

   overriding
   function Previous (Object : Buffer; Position : Cursor) return Cursor;

private

   type Buffer (Max_Capacity : Capacity_Type) is
     new Buffer_Iterator_Interfaces.Reversible_Iterator
   with record
      Inner : Core.Buffer (Max_Capacity);
   end record;

end Ring_Buffer;
