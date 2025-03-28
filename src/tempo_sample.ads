pragma Ada_2022;

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Text_Buffers;

package Tempo_Sample is
   type Sample is new Duration with Put_Image => Sample_Image;

   function Sample_Init (T : Time_Span) return Sample;

   procedure Sample_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Sample);

end Tempo_Sample;
