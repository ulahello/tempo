pragma Ada_2022;

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;
use Ada.Strings;

with Tempo_Config;
with Tempo_Tapper; use Tempo_Tapper;

procedure Tempo is
   --  TODO: surely i dont have to copy paste this
   Crate_Description : constant String := "terminal tempo tapper";

   type Command is (Help, Tap, Clear, Size, Bound, Print, Quit, Invalid);
   subtype Valid_Command is Command
   with Static_Predicate => Valid_Command not in Invalid;

   function Command_Literal (C : Valid_Command) return String
   is (case C is
         when Help => "h",
         when Tap => "",
         when Clear => "c",
         when Size => "s",
         when Bound => "b",
         when Print => "p",
         when Quit => "q");

   function Command_Short_Name (C : Valid_Command) return String
   is (case C is
         when Tap => "<enter>",
         when others => Command_Literal (C));

   function Command_Long_Name (C : Valid_Command) return String is
      S : String := C'Image;
   begin
      Fixed.Translate (S, Maps.Constants.Lower_Case_Map);
      return S;
   end Command_Long_Name;

   function Command_Description (C : Valid_Command) return String
   is (case C is
         when Help => "describe commands",
         when Tap => "register a tap",
         when Clear => "clear buffer contents",
         when Size => "adjust buffer size",
         when Bound => "toggle whether buffer is bounded to size",
         when Print => "print buffer contents",
         when Quit => "quit");

   function Parse_Command (S : String) return Command is
   begin
      --  Check S against every valid command
      for C in Valid_Command loop
         if Equal_Case_Insensitive (S, Command_Long_Name (C))
           or else Equal_Case_Insensitive (S, Command_Literal (C))
         then
            return C;
         end if;
      end loop;
      return Invalid;
   end Parse_Command;

   procedure Put_Splash is
   begin
      Put_Line
        (Tempo_Config.Crate_Name
         & " "
         & Tempo_Config.Crate_Version
         & ": "
         & Crate_Description);
      Put_Line ("type ""h"" for help");
   end Put_Splash;

   procedure Put_Prompt (T : Tapper) is
   begin
      Put (T.Count, Width => 0);
      Put ("/");
      Put (T.Bounded_Capacity, Width => 0);
      Put_Line ((if T.Is_Bounded then "" else "+") & " samples in buffer");
      Put_Line (T.Bpm'Image & " BPM");

      Put (" " & (if T.Is_Recording then '*' else ';') & " ");
   end Put_Prompt;

   procedure Do_Invalid is
   begin
      Put_Line ("");
      Put_Line (" unrecognized command. try ""h"" for help.");
   end Do_Invalid;

   procedure Do_Help is
   begin
      Put_Line ("");
      for C in Valid_Command loop
         Put_Line
           (" "
            & Command_Long_Name (C)
            & " or "
            & Command_Short_Name (C)
            & ". "
            & Command_Description (C)
            & ".");
      end loop;
   end Do_Help;

   procedure Do_Size (T : in out Tapper) is
      Size         : Integer;
      Clamped_Size : Integer;
      Reported     : Natural;
   begin
      --  Read the new size
      Put_Line ("");
      Put (" new buffer size? ");
      declare
         Try_Size : constant String := Get_Line;
      begin
         --  Blank input is no-op
         if Try_Size'Length = 0 then
            return;
         end if;

         begin
            Size := Integer'Value (Try_Size);
         exception
            when E : Constraint_Error =>
               pragma Unreferenced (E);
               Put_Line (" invalid integer");
               return;
         end;
      end;

      --  Clamp to valid range
      Clamped_Size := Size;
      Clamped_Size := Integer'Max (Clamped_Size, 1);
      Clamped_Size := Integer'Min (Clamped_Size, Max_Capacity);

      --  Resize buffer
      T.Resize (Clamped_Size);
      Reported := T.Bounded_Capacity;

      --  Report if size was clamped
      if Reported /= Size then
         Put
           (" size too "
            & (if Reported < Size then "large" else "small")
            & ", clamped to ");
         Put (Reported, Width => 0);
         Put_Line ("");
      end if;
   end Do_Size;

   procedure Do_Print (T : Tapper) is
   begin
      Put_Line ("");
      Put_Line (" " & T'Image);
   end Do_Print;

   procedure Do_Quit is
   begin
      Put_Line ("");
      Put_Line (" goodbye");
      Put_Line ("");
   end Do_Quit;

   Default_Buffer_Size : constant Natural := 10;
   Default_Bounded     : constant Boolean := True;

   T : Tapper := Tapper_Init (Default_Buffer_Size, Default_Bounded);

begin

   --  Check usage
   if Argument_Count /= 0 then
      Put_Line (Standard_Error, "usage: " & Command_Name);
      Set_Exit_Status (1);
      return;
   end if;

   --  Print splash text
   Put_Splash;

   loop
      Put_Line ("");

      --  Print the BPM and buffer stats
      Put_Prompt (T);

      --  Perform command
      declare
         Input : constant String := Get_Line;
         C     : constant Command := Parse_Command (Input);
      begin
         case C is
            when Invalid =>
               Do_Invalid;

            when Help =>
               Do_Help;

            when Tap =>
               T.Tap;

            when Clear =>
               T.Clear;

            when Size =>
               Do_Size (T);

            when Bound =>
               T.Toggle_Bounded;

            when Print =>
               Do_Print (T);

            when Quit =>
               Do_Quit;
               exit;
         end case;
      end;

   end loop;

exception
   --  Handle EOF by immediately exiting.
   when E : End_Error =>
      pragma Unreferenced (E);
      Do_Quit;
end Tempo;
