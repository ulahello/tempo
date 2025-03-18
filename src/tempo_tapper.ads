pragma Ada_2022;

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Text_Buffers;

with Ring_Buffer;  use Ring_Buffer;
with Tempo_Sample; use Tempo_Sample;

package Tempo_Tapper is
   type Tapper is private;

   function Tapper_Init
     (Bounded_Capacity : Buffer_Count; Bounded : Boolean) return Tapper;
   procedure Tapper_Tap (T : in out Tapper);
   procedure Tapper_Clear (T : in out Tapper);
   procedure Tapper_Resize (T : in out Tapper; S : Buffer_Count);
   procedure Tapper_Toggle_Bounded (T : in out Tapper);
   function Tapper_Bpm (T : Tapper) return Sample;
   function Tapper_Count (T : Tapper) return Buffer_Count;
   function Tapper_Bounded_Capacity (T : Tapper) return Buffer_Count;
   function Tapper_Is_Recording (T : Tapper) return Boolean;
   function Tapper_Is_Bounded (T : Tapper) return Boolean;

private

   procedure Tapper_Buffer_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Tapper);

   type Tapper is record
      Samples          : Buffer;
      Bounded_Capacity : Buffer_Count;
      Bounded          : Boolean;
      Recording        : Boolean;
      Last_Tap         : Time;
   end record
   with
     --  TODO: move this into public declaration
     Put_Image => Tapper_Buffer_Image;

end Tempo_Tapper;
