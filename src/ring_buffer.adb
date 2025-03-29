pragma Ada_2022;

--  TODO: tests and/or proof

package body Ring_Buffer is

   --  Internals

   function Is_Power_Of_Two (N : Natural) return Boolean is
   begin
      --  TODO: silly but correct
      for I in 0 .. Natural'Size - 1 loop
         if 2 ** I = N then
            return True;
         end if;
      end loop;
      return False;
   end Is_Power_Of_Two;

   --  The Read and Write indices are 0-based because the math is
   --  nicer, but since I can't specify the array index as
   --  `(0 .. Discriminant - 1)`, the actual array is 1-based, so I
   --  have to translate here.
   function Mask (Max_Capacity : Capacity_Type; I : Ring_Index) return Natural
   is (1 + Integer (I mod Ring_Index (Max_Capacity)));

   procedure Pop_Unchecked (B : in out Buffer) is
   begin
      B.Read := B.Read + 1;
   end Pop_Unchecked;

   procedure Push_Unchecked (B : in out Buffer; V : Element) is
   begin
      B.Memory (Mask (B.Max_Capacity, B.Write)) := V;
      B.Write := B.Write + 1;
   end Push_Unchecked;

   --  Public interface

   function Buffer_Init (Max_Capacity : Capacity_Type) return Buffer is
      Buffer_Empty : constant Buffer :=
        (Max_Capacity => Max_Capacity,
         Memory       => [others => <>],
         Read         => 0,
         Write        => 0);
   begin
      return Buffer_Empty;
   end Buffer_Init;

   function Length (B : Buffer) return Natural
   is (Natural (B.Write - B.Read));

   function Is_Empty (B : Buffer) return Boolean
   is (B.Read = B.Write);

   function Is_Full (B : Buffer) return Boolean
   is (B.Length = B.Max_Capacity);

   function Get (B : Buffer; I : Natural) return Element is
   begin
      if I >= B.Length then
         raise Constraint_Error with "buffer index out of bounds";
      end if;
      return B.Memory (Mask (B.Max_Capacity, B.Read + Ring_Index (I)));
   end Get;

   procedure Push (B : in out Buffer; V : Element) is
   begin
      --  Make room for the new element
      if B.Is_Full then
         --  The capacity is nonzero so the buffer is not empty
         B.Pop_Unchecked;
      end if;

      B.Push_Unchecked (V);
   end Push;

   function Pop (B : in out Buffer) return Element is
   begin
      if B.Is_Empty then
         raise Constraint_Error with "tried to pop from empty buffer";
      end if;
      B.Pop_Unchecked;
      return B.Memory (Mask (B.Max_Capacity, B.Read - 1));
   end Pop;

   procedure Clear (B : in out Buffer) is
   begin
      B.Write := B.Read;
   end Clear;

   procedure Truncate_Back (B : in out Buffer; Length : Natural) is
   begin
      if Length < B.Length then
         B.Read := B.Write - Ring_Index (Length);
      end if;
   end Truncate_Back;

   --  Iterable container implementation

   function Has_Element (Position : Cursor) return Boolean
   is (Position.Index in 0 .. Position.Length - 1);

   function Iterate
     (Container : Buffer)
      return Buffer_Iterator_Interfaces.Reversible_Iterator'Class
   is (Container);

   function Get (Container : Buffer; Position : Cursor) return Element
   is (Container.Get (Position.Index));

   overriding
   function First (Object : Buffer) return Cursor
   is (Index => 0, Length => Object.Length);

   overriding
   function Last (Object : Buffer) return Cursor
   is (Index => Object.Length - 1, Length => Object.Length);

   overriding
   function Next (Object : Buffer; Position : Cursor) return Cursor
   is (Index => Position.Index + 1, Length => Position.Length);

   overriding
   function Previous (Object : Buffer; Position : Cursor) return Cursor
   is (Index => Position.Index - 1, Length => Position.Length);

end Ring_Buffer;
