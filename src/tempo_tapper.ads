--  SPDX-License-Identifier: MPL-2.0

pragma Ada_2022;

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Text_Buffers;

with Ring_Buffer;
with Ring_Buffer_Core;

package Tempo_Tapper is
   type Sample is new Duration with Put_Image => Sample_Image;

   function Sample_Init (T : Time_Span) return Sample;

   procedure Sample_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Sample);

   --  Generic packages can't be directly proven[1]. Since Ring_Buffer
   --  cannot be SPARK (its Buffer extends Reversible_Iterator), its
   --  instantiation of Ring_Buffer_Core will not be checked. Instead, I
   --  artificially instantiate the package.
   --
   --  [1]: https://docs.adacore.com/spark2014-docs/html/lrm/generic-units.html
   package Please_Prove_Me
     with SPARK_Mode => On
   is
      package Core is new
        Ring_Buffer_Core
          (Element             => Sample,
           Element_Placeholder => Sample (0));
   end Please_Prove_Me;

   package Ring_Buffer_Instantiated is new
     Ring_Buffer (Element => Sample, Element_Placeholder => Sample (0));
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
