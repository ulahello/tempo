with Ada.Iterator_Interfaces;

--  TODO: i should not implement a ring buffer / queue and instead
--  should use an existing implementation... which exists, right?

generic
   type Element is private;
package Ring_Buffer is
   --  TODO: make this private
   function Is_Power_Of_Two (N : Natural) return Boolean;

   subtype Capacity_Type is Natural
   with Dynamic_Predicate => Is_Power_Of_Two (Capacity_Type);

   --  Buffer is an iterator
   type Cursor is private;
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

   type Element_Array is array (Positive range <>) of Element;

   type Ring_Index is mod Natural'Last;

   type Buffer (Max_Capacity : Capacity_Type) is
     new Buffer_Iterator_Interfaces.Reversible_Iterator
   with record
      Memory : Element_Array (1 .. Max_Capacity);
      Read   : Ring_Index;
      Write  : Ring_Index;
   end record;

   type Cursor is record
      Index  : Natural;
      Length : Natural;
   end record;

end Ring_Buffer;
