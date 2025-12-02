--  SPDX-License-Identifier: MPL-2.0

pragma Ada_2022;

with Ada.Strings.Fixed;
with System;

package body Tempo_Tapper is

   --  Samples

   function Sample_Init (T : Time_Span) return Sample
   is (60.0 / Sample (To_Duration (T)));

   procedure Sample_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Sample)
   is
      --  TODO: it should be systematically impossible for the
      --  conversion to fail (i.e. use floats and find a crate that
      --  formats rounded values)
      use Ada.Strings.Fixed;
      type Rounded is delta 10.0 ** (-1) digits System.Max_Digits;
   begin
      --  TODO: don't emit the extraneous space in the first place
      Output.Put (Trim (Rounded (Value)'Image, Ada.Strings.Left));
   end Sample_Image;

   --  Internals

   procedure Sync_Capacity (T : in out Tapper) is
   begin
      if T.Is_Bounded then
         T.Samples.Truncate_Back (T.Bounded_Capacity);
      end if;
   end Sync_Capacity;

   procedure Tapper_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Tapper) is
   begin
      Output.Put ("[");
      for C in reverse Value.Samples.Iterate loop
         declare
            S : constant Sample := Value.Samples.Get (C);
         begin
            Output.Put (S'Image);
         end;
         if C /= Value.Samples.First then
            Output.Put (", ");
         end if;
      end loop;
      Output.Put ("]");
   end Tapper_Image;

   --  Public interface

   function Tapper_Init
     (Bounded_Capacity : Natural; Bounded : Boolean) return Tapper
   is (Samples   => Buffer_Init (Max_Capacity),
       Capacity  => Bounded_Capacity,
       Bounded   => Bounded,
       Recording => False,
       Last_Tap  => <>);

   procedure Tap (T : in out Tapper) is
      Now : constant Time := Clock;
   begin

      if T.Is_Recording then
         declare
            Elapsed : constant Time_Span := Now - T.Last_Tap;
            S       : constant Sample := Sample_Init (Elapsed);
         begin
            --  Push the new BPM sample and remove old elements
            T.Samples.Push (S);
            T.Sync_Capacity;
         end;
      end if;

      T.Last_Tap := Now;
      T.Recording := True;

   end Tap;

   procedure Clear (T : in out Tapper) is
   begin
      T.Samples.Clear;
      T.Recording := False; --  Forget the last tap
   end Clear;

   procedure Resize (T : in out Tapper; S : Natural) is
   begin
      T.Capacity := Natural'Min (S, Max_Capacity);
      T.Sync_Capacity;
   end Resize;

   procedure Toggle_Bounded (T : in out Tapper) is
   begin
      T.Bounded := not T.Bounded;
      T.Sync_Capacity;
   end Toggle_Bounded;

   function Bpm (T : Tapper) return Sample is
      A : Float := 0.0;
   begin
      --  https://www.nu42.com/2015/03/how-you-average-numbers.html
      for C in T.Samples.Iterate loop
         declare
            S : constant Sample := T.Samples.Get (C);
         begin
            A := A + (Float (S) - A) / Float (C.Index); --  index starts at 1
         end;
      end loop;
      return Sample (A);
   end Bpm;

   function Count (T : Tapper) return Natural
   is (T.Samples.Length);

   function Bounded_Capacity (T : Tapper) return Natural
   is (T.Capacity);

   function Is_Bounded (T : Tapper) return Boolean
   is (T.Bounded);

   function Is_Recording (T : Tapper) return Boolean
   is (T.Recording);

end Tempo_Tapper;
