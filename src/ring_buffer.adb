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

   procedure Buffer_Pop_Unchecked (B : in out Buffer) is
   begin
      B.Read := B.Read + 1;
   end Buffer_Pop_Unchecked;

   procedure Buffer_Push_Unchecked (B : in out Buffer; V : Element) is
   begin
      B.Memory (Mask (B.Max_Capacity, B.Write)) := V;
      B.Write := B.Write + 1;
   end Buffer_Push_Unchecked;

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

   function Buffer_Length (B : Buffer) return Natural
   is (Natural (B.Write - B.Read));

   function Buffer_Is_Empty (B : Buffer) return Boolean
   is (B.Read = B.Write);

   function Buffer_Is_Full (B : Buffer) return Boolean
   is (Buffer_Length (B) = B.Max_Capacity);

   function Buffer_Get (B : Buffer; I : Natural) return Element is
   begin
      if I >= Buffer_Length (B) then
         raise Constraint_Error with "buffer index out of bounds";
      end if;
      return B.Memory (Mask (B.Max_Capacity, B.Read + Ring_Index (I)));
   end Buffer_Get;

   procedure Buffer_Push (B : in out Buffer; V : Element) is
   begin
      --  Make room for the new element
      if Buffer_Is_Full (B) then
         --  The capacity is nonzero so the buffer is not empty
         Buffer_Pop_Unchecked (B);
      end if;

      Buffer_Push_Unchecked (B, V);
   end Buffer_Push;

   function Buffer_Pop (B : in out Buffer) return Element is
   begin
      if Buffer_Is_Empty (B) then
         raise Constraint_Error with "tried to pop from empty buffer";
      end if;
      Buffer_Pop_Unchecked (B);
      return B.Memory (Mask (B.Max_Capacity, B.Read - 1));
   end Buffer_Pop;

   procedure Buffer_Clear (B : in out Buffer) is
   begin
      B.Write := B.Read;
   end Buffer_Clear;

   procedure Buffer_Truncate_Back (B : in out Buffer; Length : Natural) is
   begin
      if Length < Buffer_Length (B) then
         B.Read := B.Write - Ring_Index (Length);
      end if;
   end Buffer_Truncate_Back;

end Ring_Buffer;
