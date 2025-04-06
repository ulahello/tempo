--  SPDX-License-Identifier: MPL-2.0

pragma Ada_2022;

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Text_Buffers;

with Ring_Buffer;

package Tempo_Tapper is
   --  Sample type

   type Sample is new Duration with Put_Image => Sample_Image;

   function Sample_Init (T : Time_Span) return Sample;

   procedure Sample_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Sample);

   --  Tempo tapper collects samples

   package Ring_Buffer_Instantiated is new Ring_Buffer (Element => Sample);
   use Ring_Buffer_Instantiated;

   Max_Capacity : Capacity_Type := 16#1000#;

   type Tapper is tagged private with Put_Image => Tapper_Image;

   function Tapper_Init
     (Bounded_Capacity : Natural; Bounded : Boolean) return Tapper;

   procedure Tap (T : in out Tapper);
   procedure Clear (T : in out Tapper);
   procedure Resize (T : in out Tapper; S : Natural);
   procedure Toggle_Bounded (T : in out Tapper);
   function Bpm (T : Tapper) return Sample;
   function Count (T : Tapper) return Natural;
   function Bounded_Capacity (T : Tapper) return Natural;
   function Is_Bounded (T : Tapper) return Boolean;
   function Is_Recording (T : Tapper) return Boolean;

   procedure Tapper_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Tapper);

private

   type Tapper is tagged record
      Samples   : Buffer (Max_Capacity);
      Last_Tap  : Time;
      Capacity  : Natural;
      Bounded   : Boolean;
      Recording : Boolean;
   end record;

end Tempo_Tapper;
