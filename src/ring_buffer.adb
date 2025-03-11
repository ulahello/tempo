--  TODO: tests and/or proof

package body Ring_Buffer is

   --  Internals

   function Mask (I : Ring_Index) return Buffer_Index is
      (Buffer_Index (I mod Ring_Index (Max_Capacity)));

   procedure Buffer_Pop_Unchecked (B : in out Buffer) is
   begin
      B.Read := B.Read + 1;
   end Buffer_Pop_Unchecked;

   procedure Buffer_Push_Unchecked (B : in out Buffer; V : Element) is
   begin
      B.Memory (Mask (B.Write)) := V;
      B.Write := B.Write + 1;
   end Buffer_Push_Unchecked;

   --  Public interface

   function Buffer_Length (B : Buffer) return Buffer_Count is
      (Buffer_Count (B.Write - B.Read));

   function Buffer_Is_Empty (B : Buffer) return Boolean is
      (B.Read = B.Write);

   function Buffer_Is_Full (B : Buffer) return Boolean is
      (Buffer_Length (B) = Max_Capacity);

   function Buffer_Get (B : Buffer; I : Buffer_Index) return Element is
   begin
      if Buffer_Count (I) >= Buffer_Length (B) then
         raise Constraint_Error with "buffer index out of bounds";
      end if;
      return B.Memory (Mask (B.Read + Ring_Index (I)));
   end Buffer_Get;

   procedure Buffer_Push (B : in out Buffer; V : Element) is
   begin
      --  Make room for the new element
      if Buffer_Is_Full (B) then
         --  The capacity is nonzero so the buffer is not empty
         Buffer_Pop_Unchecked (B);
      end if;

      Buffer_Push_Unchecked (B, V);
   end Buffer_Push;

   function Buffer_Pop (B : in out Buffer) return Element is
   begin
      if Buffer_Is_Empty (B) then
         raise Constraint_Error with "tried to pop from empty buffer";
      end if;
      Buffer_Pop_Unchecked (B);
      return B.Memory (Mask (B.Read - 1));
   end Buffer_Pop;

   procedure Buffer_Clear (B : in out Buffer) is
   begin
      B.Write := B.Read;
   end Buffer_Clear;

   procedure Buffer_Truncate_Back (B : in out Buffer; Length : Buffer_Count) is
   begin
      if Length < Buffer_Length (B) then
         B.Read := B.Write - Ring_Index (Length);
      end if;
   end Buffer_Truncate_Back;

end Ring_Buffer;
