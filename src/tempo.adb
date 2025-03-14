with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
--  TODO: why can't i with/use Ada.Strings to bring this into scope?
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  TODO: so many clashing names
--  - ex. Tap command, Tempo Tapper itself, and the package name
--    where it's defined
--  - ex. Sample type, package name where it's defined (this will go
--    away after generic ring buffer)
--  - ex. Buffer integer types (which of `Buffer_Length` and
--    `Buffer_Count` count the number of elements in the ring buffer?)
with Ring_Buffer; use Ring_Buffer;
with Tempo_Config;
with Tempo_Sample; use Tempo_Sample;
with Tempo_Tapper; use Tempo_Tapper;

--  TODO: use a code formatter

--  FIXME: handle EOF
--  TODO: UTF-8

procedure Tempo is
   --  TODO: surely i dont have to copy paste this
   Crate_Description : constant String := "terminal tempo tapper";

   type Command is (Help, Tap, Clear, Size, Bound, Print, Quit, Invalid);
   subtype Valid_Command is Command
     with Static_Predicate => Valid_Command not in Invalid;

   --  TODO: most of these Command ot String maps could be arrays

   function Command_Literal (C : Valid_Command) return String is
      (case C is
         when Help => "h",
         when Tap => "",
         when Clear => "c",
         when Size => "s",
         when Bound => "b",
         when Print => "p",
         when Quit => "q");

   function Command_Short_Name (C : Valid_Command) return String is
      (case C is
         when Tap => "<enter>",
         when others => Command_Literal (C));

   function Command_Long_Name (C : Valid_Command) return String is
      --  TODO: just return lowercase Image
      (case C is
         when Help => "help",
         when Tap => "tap",
         when Clear => "clear",
         when Size => "size",
         when Bound => "bound",
         when Print => "print",
         when Quit => "quit");

   function Command_Description (C : Valid_Command) return String is
      (case C is
         when Help => "describe commands",
         when Tap => "register a tap",
         when Clear => "clear buffer contents",
         when Size => "adjust buffer size",
         when Bound => "bound or unbound buffer to size",
         when Print => "print buffer contents",
         when Quit => "quit");

   function Parse_Command (S : String) return Command is
      function Equal_Case_Insensitive (Left, Right : String) return Boolean
        renames Ada.Strings.Equal_Case_Insensitive;
   begin
      --  Check S against every valid command
      for C in Valid_Command loop
         declare
            --  TODO: use bounded strings or constrain the strings
            --  (might get refactored by the TODO about rewriting enum
            --  to string functions as arrays)
            Candidates : constant array (1 .. 2) of Unbounded_String :=
              (To_Unbounded_String (Command_Long_Name (C)),
               To_Unbounded_String (Command_Literal (C)));
         begin
            for Test of Candidates loop
               if Equal_Case_Insensitive (S, To_String (Test)) then
                  return C;
               end if;
            end loop;
         end;
      end loop;
      return Invalid;
   end Parse_Command;

   Default_Buffer_Size : constant Buffer_Count := 10;
   Default_Bounded : constant Boolean := True;

   T : Tapper := Tapper_Init (Default_Buffer_Size, Default_Bounded);

begin

   --  Check usage
   if Argument_Count /= 0 then
      Put_Line (Standard_Error, "usage: " & Command_Name);
      Set_Exit_Status (1);
      return;
   end if;

   --  Print splash text
   Put_Line (Tempo_Config.Crate_Name
             & " "
             & Tempo_Config.Crate_Version
             & ": " & Crate_Description);
   Put_Line ("type ""h"" for help");

   --  TODO: factor out string fiddling and command implementations

   --  Read eval loop
   loop
      Put_Line ("");

      --  Print the BPM and buffer stats
      Put (Integer (Tapper_Count (T)), Width => 0);
      Put ("/");
      Put (Integer (Tapper_Bounded_Capacity (T)), Width => 0);
      Put_Line ((if Tapper_Is_Bounded (T) then "" else "+")
                & " samples in buffer");
      Put_Line (Sample_Image (Tapper_Bpm (T)) & " BPM");

      Put ((if Tapper_Is_Recording (T) then " * " else " ; "));
      declare
         --  Read and parse command
         Input : constant String := Get_Line;
         C : constant Command := Parse_Command (Input);
      begin
         --  Perform command
         case C is
            when Invalid =>
               Put_Line ("");
               Put_Line (" unrecognized command. try ""h"" for help.");

            when Help =>
               Put_Line ("");
               for C in Valid_Command loop
                  Put_Line (" "
                           & Command_Long_Name (C)
                           & " or "
                           & Command_Short_Name (C)
                           & ". "
                           & Command_Description (C)
                           & ".");
               end loop;

            when Tap =>
               Tapper_Tap (T);

            when Clear =>
               Tapper_Clear (T);

            when Size =>
               --  TODO: awful. make a function.
               Put_Line ("");
               Put (" new buffer size? ");
               declare
                  Try_Size : constant String := Get_Line;
                  Size : Integer;
                  Clamped_Size : Buffer_Count;
                  Reported : Buffer_Count;
               begin
                  if Try_Size'Length /= 0 then
                     begin
                        Size := Integer'Value (Try_Size);
                        Clamped_Size :=
                          Buffer_Count (Integer'Max (0,
                          Integer'Min (Integer (Max_Capacity),
                          Size)));
                        Tapper_Resize (T, Clamped_Size);
                        Reported := Tapper_Bounded_Capacity (T);

                        --  Report clamped size
                        if Integer (Reported) < Size then
                           Put (" size too large, clamped to ");
                           Put (Integer (Reported), Width => 0);
                           Put_Line ("");
                        end if;

                     exception
                        when E : Constraint_Error =>
                           --  The exception message is not
                           --  particularly helpful ("bad input"), so
                           --  we ignore it.
                           pragma Unreferenced (E);
                           Put_Line (" invalid digit found in string");
                     end;
                  end if;
               end;

            when Bound =>
               Tapper_Toggle_Bounded (T);

            when Print =>
               Put_Line ("");
               Put_Line (" " & Tapper_Buffer_Image (T));

            when Quit =>
               Put_Line ("");
               Put_Line (" goodbye");
               exit;
         end case;
      end;

   end loop;

end Tempo;
