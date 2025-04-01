--  Generic bounded ring buffer - core functionality in SPARK

generic
   type Element is private;

   --  SPARK doesn't like <> (SPARK RM 4.3(1)) so we explicitly take a
   --  placeholder value to initialize the array with.
   Element_Placeholder : Element;

package Ring_Buffer_Core with SPARK_Mode => On
is
   subtype Capacity_Type is Natural
   with Dynamic_Predicate => Is_Power_Of_Two (Capacity_Type);
   function Is_Power_Of_Two (N : Natural) return Boolean;

   type Buffer (Max_Capacity : Capacity_Type) is tagged private;

   function Buffer_Init (Max_Capacity : Capacity_Type) return Buffer;

   --  Buffer operations

   function Length (B : Buffer) return Natural;

   function Is_Empty (B : Buffer) return Boolean;

   function Is_Full (B : Buffer) return Boolean;

   function Get (B : Buffer; I : Positive) return Element;

   procedure Push (B : in out Buffer; V : Element);

   procedure Pop (B : in out Buffer; V : out Element);

   procedure Clear (B : in out Buffer);

   procedure Truncate_Back (B : in out Buffer; Length : Natural);

private

   type Element_Array is array (Positive range <>) of Element;

   type Ring_Index is mod 2 ** (Natural'Size - 1); --  Must be a power of two

   type Buffer (Max_Capacity : Capacity_Type) is tagged record
      Memory : Element_Array (1 .. Max_Capacity);
      Read   : Ring_Index;
      Write  : Ring_Index;
   end record;

end Ring_Buffer_Core;
