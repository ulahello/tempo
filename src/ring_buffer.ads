with Tempo_Sample;
use Tempo_Sample;

--  TODO: i should not implement a ring buffer / queue and instead
--  should use an existing implementation... which exists, right?

package Ring_Buffer is
   --  TODO: enforce capacity must be a power of two
   --  TODO: make this generic over max capacity
   type Buffer_Count is new Natural range 0 .. 16#1000#;
   type Buffer_Index is new Buffer_Count range 0 .. Buffer_Count'Last - 1;
   Max_Capacity : Buffer_Count := Buffer_Count'Last;

   type Buffer is private;
   --  TODO: make this generic over element
   subtype Element is Sample;

   Buffer_Empty : constant Buffer;

   --  TODO: define attribute
   function Buffer_Length (B : Buffer) return Buffer_Count;
   function Buffer_Is_Empty (B : Buffer) return Boolean;
   function Buffer_Is_Full (B : Buffer) return Boolean;
   --  TODO: dynamic constraint for index validity
   function Buffer_Get (B : Buffer; I : Buffer_Index) return Element;
   procedure Buffer_Push (B : in out Buffer; V : Element);
   function Buffer_Pop (B : in out Buffer) return Element;
   procedure Buffer_Clear (B : in out Buffer);
   procedure Buffer_Truncate_Back (B : in out Buffer; Length : Buffer_Count);

private

   type Backing_Buffer is array (Buffer_Index) of Element;

   type Ring_Index is mod Natural'Last;

   type Buffer is record
      Memory : Backing_Buffer;
      Read   : Ring_Index;
      Write  : Ring_Index;
   end record;

   Buffer_Empty : constant Buffer := (Memory => <>, Read => 0, Write => 0);

end Ring_Buffer;
