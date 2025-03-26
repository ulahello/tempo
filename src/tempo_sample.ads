pragma Ada_2022;

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Text_Buffers;

package Tempo_Sample is
   type Sample is
     new Duration
        --  TODO: i thought Sample_Image would be undefined (and thus
        --  compile error) since it is defined below, because that was
        --  consistent with my prior experience with vaguley circular
        --  type definitions. but that's not the case!! i need to read
        --  the reference, this could mean i no longer need to do awful
        --  forward declaration stuff in other places.
        --
        --  note that we *do* get an error if sample_image is declared
        --  privately.
   with Put_Image => Sample_Image;

   function Sample_Init (T : Time_Span) return Sample;

   procedure Sample_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Sample);

end Tempo_Sample;
