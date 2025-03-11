with Ada.Real_Time; use Ada.Real_Time;

package Tempo_Sample is
   type Sample is new Duration;
   function Sample_Init (T : Time_Span) return Sample;
   --  TODO: define attribute
   function Sample_Image (S : Sample) return String;
end Tempo_Sample;
