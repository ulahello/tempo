with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with System;

package body Tempo_Sample is

   function Sample_Init (T : Time_Span) return Sample
   is (60.0 / Sample (To_Duration (T)));

   --  TODO: if im always displaying samples with a scale of 1,
   --  is there a way to implicitly do this via the type system, while
   --  also maintaining precision internally?
   procedure Sample_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Sample)
   is
      --  The exact number of digits isn't important, but if it's too
      --  low (ex. 1, 2, 3 even) we are likely to hit constraint
      --  errors for high BPMs or if the key is held down.
      --
      --  TODO: bad shape, i should probably use floating point. but
      --  then i lose convenient specification of scale.
      type Rounded is delta 10.0 ** (-1) digits System.Max_Digits;
   begin
      --  TODO: don't emit the extraneous space in the first place
      Output.Put (Trim (Rounded'Image (Rounded (Value)), Ada.Strings.Left));
   end Sample_Image;

end Tempo_Sample;
