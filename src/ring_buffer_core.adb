pragma Ada_2022;

--  TODO: tests and/or proof

package body Ring_Buffer_Core
  with SPARK_Mode => On
is

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
   function Mask (B : Buffer; I : Ring_Index) return Natural
   is (1 + Integer (I mod Ring_Index (B.Max_Capacity)));

   procedure Pop_Unchecked (B : in out Buffer; V : out Element) is
      Index : constant Ring_Index := B.Read;
   begin
      B.Read := B.Read + 1;
      V := B.Memory (Mask (B, Index));
   end Pop_Unchecked;

   procedure Push_Unchecked (B : in out Buffer; V : Element) is
   begin
      B.Memory (Mask (B, B.Write)) := V;
      B.Write := B.Write + 1;
   end Push_Unchecked;

   --  Public interface

   function Buffer_Init (Max_Capacity : Capacity_Type) return Buffer is
      Buffer_Empty : constant Buffer :=
        (Max_Capacity => Max_Capacity,
         Memory       => [others => Element_Placeholder],
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
   is (Length (B) = B.Max_Capacity);

   function Get (B : Buffer; I : Positive) return Element is
   begin
      if I > Length (B) then
         raise Constraint_Error with "buffer index out of bounds";
      end if;
      return B.Memory (Mask (B, B.Read + Ring_Index (I - 1)));
   end Get;

   procedure Push (B : in out Buffer; V : Element) is
      Extraneous : Element;
   begin
      --  Make room for the new element
      if Is_Full (B) then
         --  The capacity is nonzero so the buffer is not empty
         Pop_Unchecked (B, Extraneous);
         pragma Unreferenced (Extraneous);
      end if;

      Push_Unchecked (B, V);
   end Push;

   procedure Pop (B : in out Buffer; V : out Element) is
   begin
      if Is_Empty (B) then
         raise Constraint_Error with "tried to pop from empty buffer";
      end if;
      Pop_Unchecked (B, V);
   end Pop;

   procedure Clear (B : in out Buffer) is
   begin
      B.Write := B.Read;
   end Clear;

   procedure Truncate_Back (B : in out Buffer; Max_Length : Natural) is
   begin
      if Max_Length < Length (B) then
         B.Read := B.Write - Ring_Index (Max_Length);
      end if;
   end Truncate_Back;

end Ring_Buffer_Core;
