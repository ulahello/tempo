package body Ring_Buffer is

   --  Public interface

   function Buffer_Init (Max_Capacity : Capacity_Type) return Buffer
   is ((Max_Capacity => Max_Capacity,
        Inner        => Core.Buffer_Init (Max_Capacity)));

   function Length (B : Buffer) return Natural
   is (B.Inner.Length);

   function Is_Empty (B : Buffer) return Boolean
   is (B.Inner.Is_Empty);

   function Is_Full (B : Buffer) return Boolean
   is (B.Inner.Is_Full);

   function Get (B : Buffer; I : Positive) return Element
   is (B.Inner.Get (I));

   procedure Push (B : in out Buffer; V : Element) is
   begin
      B.Inner.Push (V);
   end Push;

   function Pop (B : in out Buffer) return Element is
      V : Element;
   begin
      B.Inner.Pop (V);
      return V;
   end Pop;

   procedure Clear (B : in out Buffer) is
   begin
      B.Inner.Clear;
   end Clear;

   procedure Truncate_Back (B : in out Buffer; Length : Natural) is
   begin
      B.Inner.Truncate_Back (Length);
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
