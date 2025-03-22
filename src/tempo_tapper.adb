package body Tempo_Tapper is
   --  Internals

   procedure Tapper_Sync_Capacity (T : in out Tapper) is
   begin
      if Tapper_Is_Bounded (T) then
         Buffer_Truncate_Back (T.Samples, Tapper_Bounded_Capacity (T));
      end if;
   end Tapper_Sync_Capacity;

   procedure Tapper_Buffer_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Tapper)
   is
      L : constant Natural := Tapper_Count (Value);
   begin
      Output.Put ("[");
      for I in 1 .. L loop
         declare
            S : constant Sample := Buffer_Get (Value.Samples, L - I);
         begin
            Output.Put (Sample_Image (S));
         end;
         if I < L then
            Output.Put (", ");
         end if;
      end loop;
      Output.Put ("]");
   end Tapper_Buffer_Image;

   --  Public interface

   function Tapper_Init
     (Bounded_Capacity : Natural; Bounded : Boolean) return Tapper
   is (Samples          => Buffer_Init (Max_Capacity),
       Bounded_Capacity => Bounded_Capacity,
       Bounded          => Bounded,
       Recording        => False,
       Last_Tap         => <>);

   procedure Tapper_Tap (T : in out Tapper) is
      Now : constant Time := Clock;
   begin

      if T.Recording then
         declare
            Elapsed : constant Time_Span := Now - T.Last_Tap;
            S       : constant Sample := Sample_Init (Elapsed);
         begin
            --  Push the new BPM sample and remove old elements
            Buffer_Push (T.Samples, S);
            Tapper_Sync_Capacity (T);
         end;
      end if;

      T.Last_Tap := Now;
      T.Recording := True;

   end Tapper_Tap;

   procedure Tapper_Clear (T : in out Tapper) is
   begin
      --  Clear buffer
      Buffer_Clear (T.Samples);

      --  Forget the latest tap
      T.Recording := False;
   end Tapper_Clear;

   procedure Tapper_Resize (T : in out Tapper; S : Natural) is
   begin
      T.Bounded_Capacity := S;
      Tapper_Sync_Capacity (T);
   end Tapper_Resize;

   procedure Tapper_Toggle_Bounded (T : in out Tapper) is
   begin
      T.Bounded := not T.Bounded;
      Tapper_Sync_Capacity (T);
   end Tapper_Toggle_Bounded;

   function Tapper_Bpm (T : Tapper) return Sample is
      A : Float := 0.0;
   begin
      if Buffer_Length (T.Samples) = 0 then
         return Sample (A);
      end if;

      --  TODO: use iterators for less error-prone range? does this exist?
      --  TODO: use https://www.nu42.com/2015/03/how-you-average-numbers.html
      for I in 0 .. Integer (Buffer_Length (T.Samples)) - 1 loop
         declare
            S : constant Sample := Buffer_Get (T.Samples, I);
         begin
            A := A + Float (S);
         end;
      end loop;

      return Sample (A / Float (Buffer_Length (T.Samples)));
   end Tapper_Bpm;

   function Tapper_Count (T : Tapper) return Natural
   is (Buffer_Length (T.Samples));

   function Tapper_Bounded_Capacity (T : Tapper) return Natural
   is (Natural'Min (T.Bounded_Capacity, T.Samples.Max_Capacity));

   function Tapper_Is_Recording (T : Tapper) return Boolean
   is (T.Recording);

   function Tapper_Is_Bounded (T : Tapper) return Boolean
   is (T.Bounded);

end Tempo_Tapper;
