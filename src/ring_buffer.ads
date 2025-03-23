
--  TODO: i should not implement a ring buffer / queue and instead
--  should use an existing implementation... which exists, right?

generic
   type Element is private;
package Ring_Buffer is
   --  TODO: make this private
   function Is_Power_Of_Two (N : Natural) return Boolean;

   subtype Capacity_Type is Natural
   with Dynamic_Predicate => Is_Power_Of_Two (Capacity_Type);

   --  using 0-based indices for the modulo math
   type Buffer (Max_Capacity : Capacity_Type) is private;

   function Buffer_Init (Max_Capacity : Capacity_Type) return Buffer;

   function Buffer_Length (B : Buffer) return Natural;
   function Buffer_Is_Empty (B : Buffer) return Boolean;
   function Buffer_Is_Full (B : Buffer) return Boolean;
   --  TODO: dynamic constraint for index validity
   function Buffer_Get (B : Buffer; I : Natural) return Element;
   procedure Buffer_Push (B : in out Buffer; V : Element);
   function Buffer_Pop (B : in out Buffer) return Element;
   procedure Buffer_Clear (B : in out Buffer);
   procedure Buffer_Truncate_Back (B : in out Buffer; Length : Natural);

private

   type Element_Array is array (Positive range <>) of Element;

   type Ring_Index is mod Natural'Last;

   type Buffer (Max_Capacity : Capacity_Type) is record
      Memory : Element_Array (1 .. Max_Capacity);
      Read   : Ring_Index;
      Write  : Ring_Index;
   end record;

end Ring_Buffer;
