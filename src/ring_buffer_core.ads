--  Generic bounded ring buffer - core functionality in SPARK

generic
   type Element is private;

   --  SPARK doesn't like <> (SPARK RM 4.3(1)) so we explicitly take a
   --  placeholder value to initialize the array with.
   Element_Placeholder : Element;

package Ring_Buffer_Core with SPARK_Mode => On
is
   Max_Max_Capacity : constant := 2 ** (Natural'Size - 1);

   subtype Index_Type is Positive range 1 .. Max_Max_Capacity;
   subtype Length_Type is Natural range 0 .. Max_Max_Capacity;

   subtype Capacity_Type is Index_Type
   with Dynamic_Predicate => Is_Power_Of_Two (Capacity_Type);
   function Is_Power_Of_Two (N : Natural) return Boolean;

   type Buffer (Max_Capacity : Capacity_Type) is private;

   function Buffer_Init (Max_Capacity : Capacity_Type) return Buffer;

   --  Buffer operations

   function Length (B : Buffer) return Length_Type;

   function Is_Empty (B : Buffer) return Boolean;

   function Is_Full (B : Buffer) return Boolean;

   function Get (B : Buffer; I : Index_Type) return Element
   with Pre => I in 1 .. Length (B);

   procedure Push (B : in out Buffer; V : Element);

   procedure Pop (B : in out Buffer; V : out Element)
   with Pre => not Is_Empty (B);

   procedure Clear (B : in out Buffer);

   procedure Truncate_Back (B : in out Buffer; Max_Length : Length_Type);

private

   type Element_Array is array (Positive range <>) of Element;

   type Ring_Index is mod Max_Max_Capacity; --  Must be a power of two

   type Buffer (Max_Capacity : Capacity_Type) is record
      Memory : Element_Array (1 .. Max_Capacity);
      Read   : Ring_Index;
      Write  : Ring_Index;
   end record;

end Ring_Buffer_Core;
