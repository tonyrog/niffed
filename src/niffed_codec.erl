%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    Encode/Decode native erlang data
%%% @end
%%% Created : 10 Jun 2012 by Tony Rogvall <tony@rogvall.se>

-module(niffed_codec).

-compile(export_all).
-import(lists, [reverse/1]).
%% (current) erlang vm tag scheme
-define(_TAG_PRIMARY_SIZE,       2).
-define(_TAG_PRIMARY_MASK,       16#3).
-define(TAG_PRIMARY_HEADER,      16#0).
-define(TAG_PRIMARY_LIST,        16#1).
-define(TAG_PRIMARY_BOXED,       16#2).
-define(TAG_PRIMARY_IMMED1,      16#3).

-define(_TAG_IMMED1_SIZE,        4).
-define(_TAG_IMMED1_MASK,        16#F).
-define(_TAG_IMMED1_PID,        ((16#0 bsl ?_TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_IMMED1)).
-define(_TAG_IMMED1_PORT,       ((16#1 bsl ?_TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_IMMED1)).
-define(_TAG_IMMED1_IMMED2,     ((16#2 bsl ?_TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_IMMED1)).
-define(_TAG_IMMED1_SMALL,      ((16#3 bsl ?_TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_IMMED1)).

-define(_TAG_IMMED2_SIZE,      6).
-define(_TAG_IMMED2_MASK,      16#3F).
-define(_TAG_IMMED2_ATOM,      ((16#0 bsl ?_TAG_IMMED1_SIZE) bor ?_TAG_IMMED1_IMMED2)).
-define(_TAG_IMMED2_CATCH,     ((16#1 bsl ?_TAG_IMMED1_SIZE) bor ?_TAG_IMMED1_IMMED2)).
-define(_TAG_IMMED2_NIL,       ((16#3 bsl ?_TAG_IMMED1_SIZE) bor ?_TAG_IMMED1_IMMED2)).

%% _TAG_PRIMARY_HEADER
-define(ARITYVAL_SUBTAG,        (16#0 bsl ?_TAG_PRIMARY_SIZE)). %% TUPLE 
-define(BIN_MATCHSTATE_SUBTAG,  (16#1 bsl ?_TAG_PRIMARY_SIZE)). 
-define(POS_BIG_SUBTAG,         (16#2 bsl ?_TAG_PRIMARY_SIZE)). %% BIG: tags 2&3 
-define(NEG_BIG_SUBTAG,         (16#3 bsl ?_TAG_PRIMARY_SIZE)). %% BIG: tags 2&3 
-define(_BIG_SIGN_BIT,          (16#1 bsl ?_TAG_PRIMARY_SIZE)).
-define(REF_SUBTAG,             (16#4 bsl ?_TAG_PRIMARY_SIZE)). %% REF 
-define(FUN_SUBTAG,             (16#5 bsl ?_TAG_PRIMARY_SIZE)). %% FUN 
-define(FLOAT_SUBTAG,           (16#6 bsl ?_TAG_PRIMARY_SIZE)). %% FLOAT 
-define(EXPORT_SUBTAG,          (16#7 bsl ?_TAG_PRIMARY_SIZE)). %% FLOAT 
-define(_BINARY_XXX_MASK,       (16#3 bsl ?_TAG_PRIMARY_SIZE)).
-define(REFC_BINARY_SUBTAG,     (16#8 bsl ?_TAG_PRIMARY_SIZE)). %% BINARY 
-define(HEAP_BINARY_SUBTAG,     (16#9 bsl ?_TAG_PRIMARY_SIZE)). %% BINARY 
-define(SUB_BINARY_SUBTAG,      (16#A bsl ?_TAG_PRIMARY_SIZE)). %% BINARY 
-define(EXTERNAL_PID_SUBTAG,    (16#C bsl ?_TAG_PRIMARY_SIZE)). %% EXTERNAL_PID 
-define(EXTERNAL_PORT_SUBTAG,   (16#D bsl ?_TAG_PRIMARY_SIZE)). %% EXTERNAL_PORT 
-define(EXTERNAL_REF_SUBTAG,    (16#E bsl ?_TAG_PRIMARY_SIZE)). %% EXTERNAL_REF 

-define(_TAG_HEADER_MASK,       0x3F).
-define(_HEADER_SUBTAG_MASK,    0x3C).    %% 4 bits for subtag
-define(_HEADER_ARITY_OFFS,     6).

-define(_TAG_HEADER_ARITYVAL,	(?TAG_PRIMARY_HEADER bor ?ARITYVAL_SUBTAG)).
-define(_TAG_HEADER_FUN,	(?TAG_PRIMARY_HEADER bor ?FUN_SUBTAG)).
-define(_TAG_HEADER_POS_BIG,	(?TAG_PRIMARY_HEADER bor ?POS_BIG_SUBTAG)).
-define(_TAG_HEADER_NEG_BIG,	(?TAG_PRIMARY_HEADER bor ?NEG_BIG_SUBTAG)).
-define(_TAG_HEADER_FLOAT,	(?TAG_PRIMARY_HEADER bor ?FLOAT_SUBTAG)).
-define(_TAG_HEADER_EXPORT,	(?TAG_PRIMARY_HEADER bor ?EXPORT_SUBTAG)).
-define(_TAG_HEADER_REF,	(?TAG_PRIMARY_HEADER bor ?REF_SUBTAG)).
-define(_TAG_HEADER_REFC_BIN,	(?TAG_PRIMARY_HEADER bor ?REFC_BINARY_SUBTAG)).
-define(_TAG_HEADER_HEAP_BIN,	(?TAG_PRIMARY_HEADER bor ?HEAP_BINARY_SUBTAG)).
-define(_TAG_HEADER_SUB_BIN,	(?TAG_PRIMARY_HEADER bor ?SUB_BINARY_SUBTAG)).
-define(_TAG_HEADER_EXTERNAL_PID,  (?TAG_PRIMARY_HEADER bor ?EXTERNAL_PID_SUBTAG)).
-define(_TAG_HEADER_EXTERNAL_PORT, (?TAG_PRIMARY_HEADER bor ?EXTERNAL_PORT_SUBTAG)).
-define(_TAG_HEADER_EXTERNAL_REF,  (?TAG_PRIMARY_HEADER bor ?EXTERNAL_REF_SUBTAG)).
-define(_TAG_HEADER_BIN_MATCHSTATE, (?TAG_PRIMARY_HEADER bor ?BIN_MATCHSTATE_SUBTAG)).

-define(MASK_WORD(W),  ((1 bsl (W))-1)).

%% -define(dbg(F), io:format(F)).
%% -define(dbg(F,As), io:format(F, As)).
-define(dbg(F), ok).
-define(dbg(F,As), ok).

encode_call(L,I,X) when is_integer(I), is_list(X) ->
    W = erlang:system_info(wordsize)*8,
    T = list_to_tuple(X),
    N = tuple_size(T),
    %% encode data as:
    %% <index> <argc> <argv[0]> ... <argv[argc-1]> Y <data>
    %% data start after arguments and index and argc, this is compensated
    %% for by offset_pointers in the driver
    Y1 = 0,
    Cdata = encode_elem(L,W,N,T,Y1,[],[]),
    {Index,_} = encode(L,W,I,0),
    {Argc,_}  = encode(L,W,N,0),
    <<Index:W/native, Argc:W/native, Cdata/binary>>.

encode(L,X) ->
    W = erlang:system_info(wordsize)*8,
    {Cell,Data} = encode(L,W,X,0),
    <<Cell:W/native, Data/binary>>.

encode(_L,W,X,Y) when is_integer(X) ->
    if W =:= 32, X >= -(1 bsl 27),  X < (1 bsl 27) ->
	    Cell = ((X band 16#0fffffff) bsl ?_TAG_IMMED1_SIZE) bor
		?_TAG_IMMED1_SMALL,
	    {Cell, <<>>};
       W =:= 64, X >= -(1 bsl 59),  X < (1 bsl 59) ->
	    Cell = ((X band 16#0fffffffffffffff) bsl ?_TAG_IMMED1_SIZE) bor
		?_TAG_IMMED1_SMALL,
	    {Cell, <<>>};
       true ->
	    {Tag,Bdata} = bignum_digits(W,X),
	    Arity = word_size(W,Bdata),
	    Cell = (?TAG_PRIMARY_BOXED bor (Y bsl 2)),
	    Data = <<(Tag bor (Arity bsl ?_HEADER_ARITY_OFFS)):W/native,
		     Bdata/binary>>,
	    {Cell, Data}
    end;
encode(_L,_W,X,_Y) when is_pid(X) ->
    encode_local_pid(X);
encode(_L,_W,X,_Y) when is_port(X) ->
    encode_local_port(X);
encode(_L,W,X,Y) when is_reference(X) ->
    encode_local_ref(W,X,Y);
encode(_L,W,X,Y) when is_float(X) ->
    Cell = (?TAG_PRIMARY_BOXED bor (Y bsl 2)),
    Arity = (64 div W),  %% 2 for 32 bit and 1 for 64 bit
    Data = <<(?_TAG_HEADER_FLOAT bor (Arity bsl ?_HEADER_ARITY_OFFS)):W/native,
	     X:64/float-native>>,
    { Cell, Data };
encode(_L,W,X,Y) when is_binary(X) ->
    Cell  = (?TAG_PRIMARY_BOXED bor (Y bsl 2)),
    Size  = byte_size(X),
    Arity = word_size(W,X)+1,  %% cover binary data and size
    K     = (W bsr 3),
    PadBytes = (K - Size) band (K-1), 
    Data = <<(?_TAG_HEADER_HEAP_BIN bor 
		  (Arity bsl ?_HEADER_ARITY_OFFS)):W/native,
	     Size:W/native,
	     X/binary, 0:PadBytes/unit:8>>,
    {Cell, Data};
encode(L,W,X,Y) when is_bitstring(X) ->
    %% idea here is to pad the bitstring and generate a binary
    %% that we can refer to with a SUB_BIN
    BitSize = bit_size(Y),
    PadBits = (8 - BitSize) band 7,
    Xb      = <<X/bits, 0:PadBits>>,
    Size    = byte_size(Xb),
    {Orig, Odata} = encode(L,W,Xb,Y+4),
    Cell  = (?TAG_PRIMARY_BOXED bor (Y bsl 2)),
    Arity = 3,
    Offs  = 0,
    BitOffs = 0,
    IsWritable = 0,
    HPad = W - 24,
    Data = <<(?_TAG_HEADER_SUB_BIN bor 
		  (Arity bsl ?_HEADER_ARITY_OFFS)):W/native,
	     Size:W/native,   %% binary size in bytes
	     Offs:W/native,   %% offset into original binary
	     (BitSize band 7),(BitOffs),(IsWritable),
	     0:HPad,
	     Orig:W/native,
	     Odata/binary>>,
    {Cell, Data};
encode(L,_W,X,_Y) when is_atom(X) ->
    Cell = L(X),
    {Cell, <<>>};
encode(_L,W,[], _Y) ->
    Cell = (((bnot 0) bsl ?_TAG_IMMED2_SIZE) band ?MASK_WORD(W)) bor
	?_TAG_IMMED2_NIL,
    Data = <<>>,
    { Cell , Data };
encode(L,W,[H|T],Y) ->
    Y1 = Y+2,
    {Hw,Hdata} = encode(L,W,H,Y1),
    {Tw,Tdata} = encode(L,W,T,Y1+word_size(W,Hdata)),
    Cell = ?TAG_PRIMARY_LIST bor (Y bsl 2),
    Data = <<Hw:W/native, Tw:W/native, Hdata/binary, Tdata/binary>>,
    { Cell, Data };
encode(L,W,T,Y) when is_tuple(T) ->
    N = tuple_size(T),
    Y1 = Y+(N+1),
    Tdata = encode_elem(L,W,N,T,Y1,[],[]),
    Cell = ?TAG_PRIMARY_BOXED bor (Y bsl 2),
    Data = << (?_TAG_HEADER_ARITYVAL + 
		   (N bsl ?_HEADER_ARITY_OFFS)):W/native,
	      Tdata/binary>>,
    {Cell,Data}.

%% do the tuple in reverse 
encode_elem(_L,_W,0, _T, _Y, Ws, Ds) ->
    list_to_binary(Ws++reverse(Ds));
encode_elem(L,W,I, T, Y, Ws, Ds) ->
    X = element(I, T),
    {Ew,Ed} = encode(L,W,X,Y),
    encode_elem(L,W,I-1,T,Y+word_size(W,Ed),[<<Ew:W/native>>|Ws],[Ed|Ds]).

%%
%% Data returned is {[<mark>], object}
%% the first list pointer will point to the last
%% data (cell) in the object
%%
%%     +----------------+
%% ->  | 2   | ARITYVAL |
%%     +----------------+
%%     |     |  LIST    |  -+
%%     +----------------+   |
%%     |     OBJ        |   |
%%     +----------------+   |
%%     | object data    |   |
%%     |      ...       |   |
%%     +----------------+   |
%%                        <-+
%% 

decode(Data) when is_binary(Data) ->
    Wb = erlang:system_info(wordsize),
    decode(Data, Wb).

decode(Data0, Wb) ->
    _V1={arityval,2}      = decode_word(Data0,0,Wb),
    _V2={list,PointerEnd} = decode_word(Data0,Wb,Wb),
    Base = PointerEnd - byte_size(Data0),
    ?dbg("sizeof(Data0) = ~w\n", [byte_size(Data0)]),
    ?dbg("V1=~p\n", [_V1]),
    ?dbg("V2=~p\n", [_V2]),
    ?dbg("Base=~p\n", [Base]),
    decode_elem(Data0,2*Wb,Base,Wb).

decode_elem(Data0,Offs,Base,Wb) ->
    D = decode_word(Data0,Offs,Wb),
    case D of
	{small,Int} -> Int;
	{nil,_Val} -> [];
	{atom,_Index} -> D; %% fixme decode atom
	{pid,Num} -> decode_local_pid(Num,Wb);
	{port,Num} -> decode_local_port(Num, Wb);
	{arityval,N} -> 
	    list_to_tuple([decode_elem(Data0,Offs+I*Wb,Base,Wb) ||
			      I <- lists:seq(1,N)]);
	{'fun',_N} -> D;
	{pos_big,Int} -> Int;
	{neg_big,Int} -> Int;
	{float,Float} -> Float;
	{export,_N} -> D;
	{ref,Ref} -> decode_local_ref(Ref,Wb);
	{refc_bin,_N} -> D;
	{heap_bin,Bin} -> Bin;
	{sub_bin,_N} -> D;
	{external_pid,_N} -> D;
	{extrenal_port,_N} -> D;
	{external_ref,_N} -> D;
	{bin_matchstate,_N} -> D;
	{list,Pointer} ->
	    Hd = Pointer-Base,
	    [decode_elem(Data0,Hd,Base,Wb) |decode_elem(Data0,Hd+Wb,Base,Wb)];
	{boxed,Pointer} -> decode_elem(Data0,Pointer-Base,Base,Wb)
    end.

decode_word(Data, Offset, Wb) ->
    <<_:Offset/binary,Word:Wb/unit:8-native, Data1/binary>> = Data,
    case Word band 16#3 of
	?TAG_PRIMARY_HEADER ->
	    case Word band 16#3f of
		?_TAG_HEADER_ARITYVAL -> 
		    {arityval,Word bsr 6};
		?_TAG_HEADER_FUN ->
		    {'fun',Word bsr 6};
		?_TAG_HEADER_POS_BIG ->
		    Size = (Word bsr 6)*Wb,
		    <<Int:Size/little-unsigned-unit:8,_/binary>> = Data1,
		    {pos_big,Int};
		?_TAG_HEADER_NEG_BIG ->
		    Size = (Word bsr 6)*Wb,
		    <<Int:Size/little-unsigned-unit:8,_/binary>> = Data1,
		    {neg_big,-Int};
		?_TAG_HEADER_FLOAT ->
		    Size = (Word bsr 6)*Wb,
		    <<Float:Size/native-float-unit:8,_/binary>> = Data1,
		    {float,Float};
		?_TAG_HEADER_EXPORT -> 
		    {export,Word bsr 6};
		?_TAG_HEADER_REF -> 
		    Size = (Word bsr 6)*Wb,
		    <<Ref:Size/binary,_/binary>> = Data1,
		    {ref,Ref};
		?_TAG_HEADER_REFC_BIN -> 
		    {refc_bin,Word bsr 6};
		?_TAG_HEADER_HEAP_BIN -> 
		    <<BinSize:Wb/native-unit:8,
		      Bin:BinSize/binary,_/binary>> = Data1,
		    {heap_bin,Bin};
		?_TAG_HEADER_SUB_BIN -> 
		    {sub_bin,Word bsr 6};
		?_TAG_HEADER_EXTERNAL_PID -> 
		    {external_pid,Word bsr 6};
		?_TAG_HEADER_EXTERNAL_PORT ->
		    {external_port,Word bsr 6};
		?_TAG_HEADER_EXTERNAL_REF  ->
		    {external_ref,Word bsr 6};
		?_TAG_HEADER_BIN_MATCHSTATE ->
		    {bin_matchstate,Word bsr 6}
	    end;
	?TAG_PRIMARY_LIST ->
	    {list,Word band (bnot 3)};
	?TAG_PRIMARY_BOXED ->
	    {boxed,Word band (bnot 3)};
	?TAG_PRIMARY_IMMED1 ->
	    case Word band 16#f of
		?_TAG_IMMED1_PID -> {pid,Word bsr 4};
		?_TAG_IMMED1_PORT -> {port,Word bsr 4};
		?_TAG_IMMED1_IMMED2 ->
		    case Word band 16#3f of
			?_TAG_IMMED2_ATOM -> {atom,Word bsr 6};
			?_TAG_IMMED2_CATCH -> {'catch',Word};
			?_TAG_IMMED2_NIL -> {nil,Word bsr 6}
		    end;
		?_TAG_IMMED1_SMALL ->
		    Ws = 8*Wb,
		    if (Word bsr (Ws-1)) band 1 =:= 1 -> %% negative
			    Int = ((bnot Word) bsr 4) band ((1 bsl (Ws-4))-1),
			    {small,-(Int + 1)};
		       true ->
			    {small,(Word bsr 4)}
		    end
	    end
    end.


bignum_digits(W,Num) when Num < 0 ->
    {?_TAG_HEADER_NEG_BIG, bignum_digits(W,-Num, <<>>)};
bignum_digits(W,Num) when Num > 0 ->
    {?_TAG_HEADER_POS_BIG, bignum_digits(W,Num, <<>>)};
bignum_digits(W,0) ->
    {?POS_BIG_SUBTAG, <<0:W/native>>}.
    
bignum_digits(_W,0,Bdata) -> 
    Bdata;
bignum_digits(W,N,Bdata) -> 
    bignum_digits(W,N bsr W, 
		  <<Bdata/binary,(N band ?MASK_WORD(W)):W/native>>).

word_size(32,Bin) ->
    (byte_size(Bin)+3) div 4;
word_size(64,Bin) ->
    (byte_size(Bin)+7) div 8.

%% Helpers that cheat using external format
-define(VERSION_MAGIC, 131).

-define(SMALL_INTEGER_EXT,   $a).
-define(INTEGER_EXT,         $b).
-define(FLOAT_EXT,           $c).
-define(ATOM_EXT,            $d).  %% 100
-define(SMALL_ATOM_EXT,      $s).
-define(REFERENCE_EXT,       $e).
-define(NEW_REFERENCE_EXT,   $r).  %% 114
-define(PORT_EXT,            $f).  %% 102
-define(NEW_FLOAT_EXT,       $F).
-define(PID_EXT,             $g).  %% 103
-define(SMALL_TUPLE_EXT,     $h).
-define(LARGE_TUPLE_EXT,     $i).
-define(NIL_EXT,             $j).
-define(STRING_EXT,          $k).
-define(LIST_EXT,            $l).
-define(BINARY_EXT,          $m).
-define(BIT_BINARY_EXT,      $M).
-define(SMALL_BIG_EXT,       $n).
-define(LARGE_BIG_EXT,       $o).
-define(NEW_FUN_EXT,         $p).
-define(EXPORT_EXT,          $q).
-define(FUN_EXT,             $u).
-define(ATOM_UTF8_EXT,       $v).
-define(SMALL_ATOM_UTF8_EXT, $w).
-define(MAP_EXT,             $t).

encode_local_port(Port) when is_port(Port) ->
    <<?VERSION_MAGIC,
      ?PORT_EXT,
      ?ATOM_EXT,N:16,_Node:N/binary,
      ID:32, _Creation:8>> = term_to_binary(Port),
    ?dbg("Port: Node=~s,Creation=~w,ID=~w, \n", [_Node,_Creation,ID]),
    Cell = (ID bsl ?_TAG_IMMED1_SIZE) bor ?_TAG_IMMED1_PORT,
    {Cell, <<>>}.

decode_local_port(Num, _Wb) ->
    Node = atom_to_binary(node(), latin1),
    N = byte_size(Node),
    _Creation = 0,
    Bin = <<?VERSION_MAGIC,?PORT_EXT,?ATOM_EXT,N:16,Node:N/binary,
	    Num:32, _Creation:8>>,
    binary_to_term(Bin).

encode_local_pid(Pid) when is_pid(Pid) ->
    <<?VERSION_MAGIC,
      ?PID_EXT,
      ?ATOM_EXT,N:16,_Node:N/binary,
      Num:32,Serial:32,_Creation:8>> = term_to_binary(Pid),
    ?dbg("Pid: Node=~s,Creation=~w,Num=~w,Serial=~w \n", 
	 [_Node,_Creation,Num,Serial]),
    Cell = (((Serial bsl 15) bor Num) bsl ?_TAG_IMMED1_SIZE) bor
	?_TAG_IMMED1_PID,
    {Cell, <<>>}.

decode_local_pid(NumSer, _Wb) ->
    Node = atom_to_binary(node(), latin1),
    N = byte_size(Node),
    Num = NumSer band 16#7fff,  %% 15 bit pid number
    Serial = (NumSer bsr 15) band 16#1fff,  %% 13 bit pid serial
    _Creation = 0,
    Bin = <<?VERSION_MAGIC,?PID_EXT,?ATOM_EXT,N:16,Node:N/binary,
	    Num:32,Serial:32,_Creation:8>>,
    binary_to_term(Bin).


encode_local_ref(W,Ref,Y) when is_reference(Ref) ->
    <<?VERSION_MAGIC,
      ?NEW_REFERENCE_EXT,
      Len:16,
      ?ATOM_EXT,N:16,_Node:N/binary,
      _Creation:8,ID:Len/unit:32-binary>> = term_to_binary(Ref),
    ?dbg("Ref: Node=~s,Creation=~w,ID=~w, \n", [_Node,_Creation,ID]),
    Cell = (?TAG_PRIMARY_BOXED bor (Y bsl 2)),
    Size = bit_size(ID),
    Arity = (Size+W-1) div W,
    Pad = Arity*W - Size,
    Data = <<(?_TAG_HEADER_REF bor 
		  (Arity bsl ?_HEADER_ARITY_OFFS)):W/native,
	     0:Pad, ID/binary>>,
    {Cell, Data}.

decode_local_ref(<<0,0,0,0,Ref/binary>>, 8) -> decode_local_ref_(Ref);
decode_local_ref(Ref, 4) ->    decode_local_ref_(Ref);
decode_local_ref(Ref, _Wb) ->  decode_local_ref_(Ref).

%% use external format to re-create the reference
decode_local_ref_(Ref) ->
    Node = atom_to_binary(node(), latin1),
    N = byte_size(Node),
    Len  = byte_size(Ref) div 4,
    _Creation = 0,
    Bin = <<?VERSION_MAGIC,
	    ?NEW_REFERENCE_EXT,
	    Len:16,
	    ?ATOM_EXT,N:16,Node/binary,
	    _Creation:8,Ref/binary>>,
    ?dbg("binary = ~w\n", [Bin]),
    binary_to_term(Bin).
