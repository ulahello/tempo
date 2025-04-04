--  SPDX-License-Identifier: MPL-2.0

pragma Ada_2022;

--  TODO: tests and/or proof

package body Ring_Buffer is

   --  Public interface

   function Buffer_Init (Max_Capacity : Capacity_Type) return Buffer
   is ((Max_Capacity => Max_Capacity,
        Inner        => Core.Buffer_Init (Max_Capacity)));

   function Length (B : Buffer) return Natural
   is (Core.Length (B.Inner));

   function Is_Empty (B : Buffer) return Boolean
   is (Core.Is_Empty (B.Inner));

   function Is_Full (B : Buffer) return Boolean
   is (Core.Is_Full (B.Inner));

   function Get (B : Buffer; I : Positive) return Element is
   begin
      if I not in 1 .. B.Length then
         raise Constraint_Error with "buffer index out of bounds";
      end if;
      return Core.Get (B.Inner, I);
   end Get;

   procedure Push (B : in out Buffer; V : Element) is
   begin
      Core.Push (B.Inner, V);
   end Push;

   function Pop (B : in out Buffer) return Element is
      V : Element;
   begin
      if B.Is_Empty then
         raise Constraint_Error with "tried to pop from empty buffer";
      end if;
      Core.Pop (B.Inner, V);
      return V;
   end Pop;

   procedure Clear (B : in out Buffer) is
   begin
      Core.Clear (B.Inner);
   end Clear;

   procedure Truncate_Back (B : in out Buffer; Max_Length : Natural) is
   begin
      Core.Truncate_Back (B.Inner, Max_Length);
   end Truncate_Back;

   --  Iterable container implementation

   function Has_Element (Position : Cursor) return Boolean
   is (Position.Index in 1 .. Position.Length);

   function Iterate
     (Container : Buffer)
      return Buffer_Iterator_Interfaces.Reversible_Iterator'Class
   is (Container);

   function Get (Container : Buffer; Position : Cursor) return Element
   is (Container.Get (Position.Index));

   overriding
   function First (Object : Buffer) return Cursor
   is (Index => 1, Length => Object.Length);

   overriding
   function Last (Object : Buffer) return Cursor
   is (Index => Object.Length, Length => Object.Length);

   overriding
   function Next (Object : Buffer; Position : Cursor) return Cursor
   is (Index => Position.Index + 1, Length => Position.Length);

   overriding
   function Previous (Object : Buffer; Position : Cursor) return Cursor
   is (Index => Position.Index - 1, Length => Position.Length);

end Ring_Buffer;
