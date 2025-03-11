with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Tempo_Sample is
   function Sample_Init (T : Time_Span) return Sample is
      (60.0 / Sample (To_Duration (T)));

   --  TODO: if im always displaying samples with two decimal places,
   --  is there a way to implicitly do this via the type system, while
   --  also maintaining precision internally?
   function Sample_Image (S : Sample) return String is
      --  TODO: i only care about rounding to 2 decimal places, not
      --  the overall precision (i am overspecifying)
      type Rounded is delta 10.0**(-1) digits 12;
   begin
      --  TODO: don't emit the extraneous space in the first place
      return Trim (Rounded'Image (Rounded (S)), Ada.Strings.Left);
   end Sample_Image;
end Tempo_Sample;
