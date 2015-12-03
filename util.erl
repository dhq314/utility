%%%-----------------------------------
%%% @doc 工具函数集合
%%%-----------------------------------
-module(util).
-compile(export_all).

-export([bin/2, get_include_file_name/1]).

%% 列表处理函数
-export([set_nth/3]).

-define(DIFF_SECONDS_1970_1900, 2208988800).
-define(DIFF_SECONDS_0000_1900, 62167219200).
-define(ONE_HOUR_SECONDS, 3600).
-define(ONE_DAY_SECONDS, 86400).



%% --------------------------------------------------
%%                 列表工具函数
%% --------------------------------------------------
%% 在List中的每两个元素之间插入一个分隔符
implode(_S, []) ->
    [<<>>];
implode(S, L) when is_list(L) ->
    implode(S, L, []).
implode(_S, [H], NList) ->
    lists:reverse([thing_to_list(H) | NList]);
implode(S, [H | T], NList) ->
    L = [thing_to_list(H) | NList],
    implode(S, T, [S | L]).

%% 字符->列
explode(S, B) ->
    re:split(B, S, [{return, list}]).
explode(S, B, int) ->
    [list_to_integer(Str) || Str <- explode(S, B), length(Str) > 0].


%% record 转 list
%% @return 一个LIST 成员同 Record一样
record_to_list(Record) when erlang:is_tuple(Record) ->
    RecordList = erlang:tuple_to_list(Record),
    [_ | ListLeft] = RecordList,
    ListLeft;
record_to_list(_Record) ->
    [].

%% list 转 record
%% @param List 必须长度跟rocrod一样
%% @param RecordAtom record的名字,是一个原子
%% @return RecordAtom类型的RECORD
list_to_record(List, RecordAtom) ->
    RecordList = [RecordAtom | List],
    erlang:list_to_tuple(RecordList).

%%列表去重
list_filter_repeat(List) ->
    lists:foldl(fun(Id, List1) ->
        case lists:member(Id, List1) of
            true -> List1;
            false -> List1 ++ [Id]
        end
    end, [], List).

%% 扩展版lists:min/1
%% @param: (List, N), List为元组列表，N为元组中第N个元素
min_ex([H | T], N) -> min_ex(T, H, N).

min_ex([H | T], Min, N) when element(N, H) < element(N, Min) -> min_ex(T, H, N);
min_ex([_ | T], Min, N) -> min_ex(T, Min, N);
min_ex([], Min, _) -> Min.

%% 扩展版lists:min/1
%% @param: (List, N), List为元组列表，N为元组中第N个元素
max_ex([H | T], N) -> max_ex(T, H, N).

max_ex([H | T], Min, N) when element(N, H) > element(N, Min) -> max_ex(T, H, N);
max_ex([_ | T], Min, N) -> max_ex(T, Min, N);
max_ex([], Min, _) -> Min.

%% 获取两个列表的公共部分
list_the_same_path(List1, List2) ->
    F = fun(Id) ->
        lists:member(Id, List2)
    end,
    lists:filter(F, List1).

%% 随机从集合中选出指定个数的元素length(List) >= Num
%% [1,2,3,4,5,6,7,8,9]中选出三个不同的数字[1,2,4]
get_random_list(List, Num) ->
    ListSize = length(List),
    if
        ListSize =< Num ->
            List;
        true ->
            F = fun(N, List1) ->
                Random = rand(1, (ListSize - N + 1)),
                Elem = lists:nth(Random, List1),
                List2 = lists:delete(Elem, List1),
                List2
            end,
            Result = lists:foldl(F, List, lists:seq(1, Num)),
            List -- Result
    end.

%% 随机打乱list元素顺序
list_shuffle(L) ->
    Len = length(L),
    List1 = [{rand(1, Len + 10000), X} || X <- L],
    List2 = lists:sort(List1),
    [E || {_, E} <- List2].

%% 查找ListOfList中某个字段值为Key的结果
%% @spec keyfind(Key, N, ListList) -> false | Value
keyfind(_, _, []) ->
    false;
keyfind(Key, N, List) ->
    [List1 | NewList] = List,
    case lists:nth(N, List1) of
        Key ->
            List1;
        _ ->
            keyfind(Key, N, NewList)
    end.

%% 扩展的map函数
map_ex(_Fun, [], _Arg) ->
    [];
map_ex(Fun, [H | T], Arg) ->
    [Fun(H, Arg) | map_ex(Fun, T, Arg)].

%% 根据lists的元素值获得下标
get_list_elem_index(Elem, List) ->
    get_lists_elem_index_helper(List, Elem, 0).
get_lists_elem_index_helper([], _Elem, _Index) ->
    0;
get_lists_elem_index_helper([H | T], Elem, Index) ->
    if H =:= Elem ->
        Index + 1;
        true ->
            get_lists_elem_index_helper(T, Elem, Index + 1)
    end.

%% 增加lists相应元素的值并获得新lists
replace_list_elem(Index, NewElem, List) ->
    replace_list_elem_helper(List, Index, NewElem, 1, []).
replace_list_elem_helper([], _Index, _NewElem, _CurIndex, NewList) ->
    NewList;
replace_list_elem_helper([H | T], Index, NewElem, CurIndex, NewList) ->
    if Index =:= CurIndex ->
        replace_list_elem_helper(T, Index, NewElem, CurIndex + 1, NewList ++ [NewElem]);
        true ->
            replace_list_elem_helper(T, Index, NewElem, CurIndex + 1, NewList ++ [H])
    end.

%%dict获取列表
dict_to_list([]) ->
    [];
dict_to_list(Dict) ->
    get_list(dict:to_list(Dict), []).

get_list([], L) ->
    L;
get_list([{_Key, List} | T], L) ->
    get_list(T, [List | L]).

%% for循环
for(Max, Max, F) ->
    F(Max);
for(I, Max, F) ->
    F(I),
    for(I + 1, Max, F).

%% 带返回状态的for循环
%% @return {ok, State}
for(Max, Min, _F, State) when Min < Max -> {ok, State};
for(Max, Max, F, State) -> F(Max, State);
for(I, Max, F, State) -> {ok, NewState} = F(I, State), for(I + 1, Max, F, NewState).

%% 从一个list中随机取出一项
%% null | term()
list_rand([]) -> null;
list_rand([I]) -> I;
list_rand(List) ->
    Len = length(List),
    Rand = rand(1, Len),
    lists:nth(Rand, List).

%% @doc lists 的每一项执行函数，ok则继续
list_handle(F, Data, [H | T]) ->
    case F(H, Data) of
        {ok, Data2} ->
            list_handle(F, Data2, T);
        Error ->
            Error
    end;
list_handle(_F, Data, []) ->
    {ok, Data}.

list_handle2(F, [H | T]) ->
    case F(H) of
        ok ->
            list_handle2(F, T);
        Error ->
            Error
    end;
list_handle2(_F, []) ->
    ok.

%%处理物品
list_handle_goods(_F, Data, [], InfoList) ->
    {ok, Data, InfoList};

list_handle_goods(F, Data, [H | T], InfoList) ->
    case F(H, Data) of
        {ok, Data2, Info} ->
            list_handle_goods(F, Data2, T, [Info | InfoList]);
        Error ->
            Error
    end.

%% @doc get random list
list_random([]) ->
    {};
list_random(List) ->
    RS = lists:nth(random:uniform(length(List)), List),
    ListTail = lists:delete(RS, List),
    {RS, ListTail}.

%% @doc get a random integer between Min and Max
random(Min, Max) ->
    Min2 = Min - 1,
    random:uniform(Max - Min2) + Min2.

%% 合并列表
combine_lists(L1, L2) ->
    Rtn =
        lists:foldl(
            fun(T, Acc) ->
                case lists:member(T, Acc) of
                    true ->
                        Acc;
                    false ->
                        [T | Acc]
                end
            end, lists:reverse(L1), L2),
    lists:reverse(Rtn).

%% 删除list中的第N个元素
delete_list_by_index(List, N) ->
    case N > length(List) orelse N =< 0 of
        true ->
            List;
        false ->
            {List1, List2} = lists:split(N - 1, List),
            [_ | List3] = List2,
            List1 ++ List3
    end.

%% 删除列表的所有目标元素
list_delete(List, Target) ->
    list_delete_action(List, Target, []).
list_delete_action([], _Target, L2) ->
    L2;
list_delete_action([Target | L], Target, L2) ->
    list_delete_action(L, Target, L2);
list_delete_action([Other | L], Target, L2) ->
    list_delete_action(L, Target, [Other | L2]).

%% -----------------------------------------------------------
%%                  时间相关工具函数
%% ---------------------------------------------------------
%% 取得当前的unix时间戳（秒）
unixtime() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    MegaSecs * 1000000 + Secs.

%% 取得某个时间点的unix时间戳
% LocalTime = {{Y,M,D},{H,M,S}}
unixtime(LocalTime) ->
    [UniversalTime] = calendar:local_time_to_universal_time_dst(LocalTime),
    S1 = calendar:datetime_to_gregorian_seconds(UniversalTime),
    S1 - ?DIFF_SECONDS_0000_1900.

%% 取得当天零点的unix时间戳
unixdate(UnixTime) ->
    {MegaSecs, Secs, MicroSecs} = unixtime_to_now(UnixTime),
    {_Date, Time} = calendar:now_to_local_time({MegaSecs, Secs, MicroSecs}),
    Ds = calendar:time_to_seconds(Time),
    MegaSecs * 1000000 + Secs - Ds.

unixtime_to_now(Time) ->
    MegaSecs = Time div 1000000,
    Secs = Time rem 1000000,
    {MegaSecs, Secs, 0}.

diff_day(UnixTime) ->
    {Date1, _Time1} = now_to_local_time(UnixTime),
    {Date2, _Time2} = calendar:local_time(),
    calendar:date_to_gregorian_days(Date2) - calendar:date_to_gregorian_days(Date1).

%% 精确到毫秒
longunixtime() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs * 1000000000000 + Secs * 1000000 + MicroSecs) div 1000.

%% 今天是星期几
get_day_of_week() ->
    Seconds = unixtime(),
    {{Year, Month, Day}, _Time} = seconds_to_localtime(Seconds),
    calendar:day_of_the_week(Year, Month, Day).
get_day_of_week(Seconds) ->
    {{Year, Month, Day}, _Time} = seconds_to_localtime(Seconds),
    calendar:day_of_the_week(Year, Month, Day).

%% @doc 时间戳转日期
seconds_to_localtime(Seconds) ->
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + ?DIFF_SECONDS_0000_1900),
    calendar:universal_time_to_local_time(DateTime).

%% @doc 日期转时间戳
localtime_to_seconds(Dateime) ->
    calendar:datetime_to_gregorian_seconds(Dateime) -
        calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}) - 8 * 3600.


%% 判断是否同一天
is_same_date(Seconds) ->
    Now = util:unixtime(),
    is_same_date(Now, Seconds).
is_same_date(Seconds1, Seconds2) ->
    {{Year1, Month1, Day1}, _Time1} = seconds_to_localtime(Seconds1),
    {{Year2, Month2, Day2}, _Time2} = seconds_to_localtime(Seconds2),
    if ((Year1 /= Year2) or (Month1 /= Month2) or (Day1 /= Day2)) -> false;
        true -> true
    end.

%% 判断是否同一星期
is_same_week(Seconds1, Seconds2) ->
    {{Year1, Month1, Day1}, Time1} = seconds_to_localtime(Seconds1),
    % 星期几
    Week1 = calendar:day_of_the_week(Year1, Month1, Day1),
    % 从午夜到现在的秒数
    Diff1 = calendar:time_to_seconds(Time1),
    Monday = Seconds1 - Diff1 - (Week1 - 1) * ?ONE_DAY_SECONDS,
    Sunday = Seconds1 + (?ONE_DAY_SECONDS - Diff1) + (7 - Week1) * ?ONE_DAY_SECONDS,
    if ((Seconds2 >= Monday) and (Seconds2 < Sunday)) -> true;
        true -> false
    end.

%% 获取当天0点和第二天0点
get_midnight_seconds(Seconds) ->
    {{_Year, _Month, _Day}, Time} = seconds_to_localtime(Seconds),
    % 从午夜到现在的秒数
    Diff = calendar:time_to_seconds(Time),
    % 获取当天0点
    Today = Seconds - Diff,
    % 获取第二天0点
    NextDay = Seconds + (?ONE_DAY_SECONDS - Diff),
    {Today, NextDay}.

%% 计算相差的天数
get_diff_days(Seconds1, Seconds2) ->
    {{Year1, Month1, Day1}, _Time1} = seconds_to_localtime(Seconds1),
    {{Year2, Month2, Day2}, _Time2} = seconds_to_localtime(Seconds2),
    Days1 = calendar:date_to_gregorian_days(Year1, Month1, Day1),
    Days2 = calendar:date_to_gregorian_days(Year2, Month2, Day2),
    abs(Days2 - Days1).

%% 获取当天0点到现在的秒数
get_seconds_from_midnight() ->
    Now = unixtime(),
    get_seconds_from_midnight(Now).
get_seconds_from_midnight(Seconds) ->
    {_Date, Time} = seconds_to_localtime(Seconds),
    calendar:time_to_seconds(Time).

%% 获取当天0点
get_today_midnight() ->
    NowTime = unixtime(),
    {_Date, Time} = seconds_to_localtime(NowTime),
    % 从午夜到现在的秒数
    Diff = calendar:time_to_seconds(Time),
    % 获取当天0点
    NowTime - Diff.

get_today_midnight(Seconds) ->
    {_Date, Time} = seconds_to_localtime(Seconds),
    % 从午夜到现在的秒数
    Diff = calendar:time_to_seconds(Time),
    % 获取当天0点
    Seconds - Diff.

%% 获取当前整点时间
get_now_int_time() ->
    NowTime = unixtime(),
    {{_Year, _Month, _Day}, Time} = seconds_to_localtime(NowTime),
    % 从午夜到现在的秒数
    Diff = calendar:time_to_seconds(Time),
    % 获取当天0点
    round((NowTime - Diff) + (Diff div 3600) * 3600).

%% 时间戳（秒级）转为易辨别时间字符串
%% 例如： 1291014369 -> 2010年11月29日15时6分
unixtime_to_time_string(Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = now_to_local_time(Timestamp),
    lists:concat([Year, "-", Month, "-", Day, " ", Hour, ":", Minute, ":", Second]).
now_to_local_time(Timestamp) ->
    calendar:now_to_local_time(unixtime_to_now(Timestamp)).

%% 查询一周时间范围
get_week_time() ->
    Timestamp = util:unixtime(),
    get_week_time(Timestamp).
get_week_time(Timestamp) ->
    {Date, Time} = now_to_local_time(Timestamp),
    TodaySecs = calendar:time_to_seconds(Time),
    WeekDay = calendar:day_of_the_week(Date),
    Monday = Timestamp - ?ONE_DAY_SECONDS * (WeekDay - 1) - TodaySecs,
    NextMonday = Monday + 7 * ?ONE_DAY_SECONDS,
    {Monday, NextMonday}.


%% --------------------------------------------
%%             转换工具函数
%% --------------------------------------------
%% 转换成HEX格式的md5
md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(erlang:md5(S))]).

md5_2(S) ->
    Md5_bin = erlang:md5(S),
    Md5_list = binary_to_list(Md5_bin),
    lists:flatten(list_to_hex(Md5_list)).
list_to_hex(L) ->
    [int_to_hex(X) || X <- L].

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].
hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N - 10).
%%urlencode
url_encode([H | T]) ->
    if
        H >= $a, $z >= H ->
            [H | url_encode(T)];
        H >= $A, $Z >= H ->
            [H | url_encode(T)];
        H >= $0, $9 >= H ->
            [H | url_encode(T)];
        H == $_; H == $.; H == $-; H == $/; H == $: ->
            [H | url_encode(T)];
        true ->
            case integer_to_hex(H) of
                [X, Y] ->
                    [$%, X, Y | url_encode(T)];
                [X] ->
                    [$%, $0, X | url_encode(T)]
            end
    end;
url_encode([]) ->
    [].
integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
        {'EXIT', _} ->
            old_integer_to_hex(I);
        Int ->
            Int
    end.
old_integer_to_hex(I) when I < 10 ->
    integer_to_list(I);
old_integer_to_hex(I) when I < 16 ->
    [I - 10 + $A];
old_integer_to_hex(I) when I >= 16 ->
    N = trunc(I / 16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).

%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
%%     lists:flatten(io_lib:format("~p", [Term])).
    binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
    case erl_scan:string(String ++ ".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.

%% term反序列化，string转换为term，e.g., "{a},1"  => [{a},1]
string_to_list(String) ->
    case erl_scan:string("[" ++ String ++ "].") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
    string_to_term(binary_to_list(BitString)).

bitstring_to_term(BitString, Default) ->
    case bitstring_to_term(BitString) of
        undefined -> Default;
        Term -> Term
    end.

%% IP元组转字符
ip2bin({A, B, C, D}) ->
    [integer_to_list(A), ".", integer_to_list(B), ".", integer_to_list(C), ".", integer_to_list(D)].


%% 将列里的不同类型转行成字节型，如 [<<“字节”>>, 123, asdasd, "asdasd"] 输出 <<"字节123asdasdasdasd">>
all_to_binary(List) -> all_to_binary(List, []).

all_to_binary([], Result) -> list_to_binary(Result);
all_to_binary([P | T], Result) when is_list(P) -> all_to_binary(T, lists:append(Result, P));
all_to_binary([P | T], Result) when is_integer(P) -> all_to_binary(T, lists:append(Result, integer_to_list(P)));
all_to_binary([P | T], Result) when is_binary(P) -> all_to_binary(T, lists:append(Result, binary_to_list(P)));
all_to_binary([P | T], Result) when is_float(P) -> all_to_binary(T, lists:append(Result, float_to_list(P)));
all_to_binary([P | T], Result) when is_atom(P) -> all_to_binary(T, lists:append(Result, atom_to_list(P))).

%%转换成list
thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X) -> float_to_list(X);
thing_to_list(X) when is_atom(X) -> atom_to_list(X);
thing_to_list(X) when is_binary(X) -> binary_to_list(X);
thing_to_list(X) when is_list(X) -> X.

%% @doc convert other type to list
to_list(Msg) when is_list(Msg) ->
    Msg;
to_list(Msg) when is_atom(Msg) ->
    atom_to_list(Msg);
to_list(Msg) when is_binary(Msg) ->
    binary_to_list(Msg);
to_list(Msg) when is_integer(Msg) ->
    integer_to_list(Msg);
to_list(Msg) when is_tuple(Msg) ->
    tuple_to_list(Msg);
to_list(Msg) when is_float(Msg) ->
    f2s(Msg);
to_list(_Msg) ->
    "".

%% @doc convert other type to atom
to_atom(Msg) when is_atom(Msg) ->
    Msg;
to_atom(Msg) when is_binary(Msg) ->
    list_to_atom2(binary_to_list(Msg));
to_atom(Msg) when is_list(Msg) ->
    list_to_atom2(Msg);
to_atom(_) ->
    throw(other_value).

%% @doc convert other type to binary
to_binary(Msg) when is_binary(Msg) ->
    Msg;
to_binary(Msg) when is_atom(Msg) ->
    list_to_binary(atom_to_list(Msg));
%%atom_to_binary(Msg, utf8);
to_binary(Msg) when is_list(Msg) ->
    list_to_binary(Msg);
to_binary(Msg) when is_integer(Msg) ->
    list_to_binary(integer_to_list(Msg));
to_binary(Msg) when is_float(Msg) ->
    list_to_binary(f2s(Msg));
to_binary(_Msg) ->
    <<>>.

%% @doc convert other type to float
to_float(Msg) ->
    Msg2 = to_list(Msg),
    list_to_float(Msg2).

%% @doc convert other type to integer
-spec to_integer(Msg :: any()) -> integer().
to_integer("") ->
    0;
to_integer(Msg) when is_integer(Msg) ->
    Msg;
to_integer(Msg) when is_binary(Msg) ->
    to_integer(binary_to_list(Msg));
to_integer(Msg) when is_list(Msg) ->
    case only_digits(Msg) of
        true ->
            list_to_integer(Msg);
        false ->
            0
    end;
to_integer(Msg) when is_float(Msg) ->
    round(Msg);
to_integer(Msg) when is_atom(Msg) ->
    to_integer(atom_to_list(Msg));
to_integer(_Msg) ->
    0.

only_digits([]) ->
    true;
only_digits([C | R]) when C >= $0 andalso C =< $9 ->
    only_digits(R);
only_digits(_) ->
    false.

%% @doc convert other type to bool
to_bool(D) when is_integer(D) ->
    D =/= 0;
to_bool(D) when is_list(D) ->
    length(D) =/= 0;
to_bool(D) when is_binary(D) ->
    to_bool(binary_to_list(D));
to_bool(D) when is_boolean(D) ->
    D;
to_bool(_D) ->
    throw(other_value).

%% @doc convert other type to tuple
to_tuple(T) when is_tuple(T) -> T;
to_tuple(T) -> {T}.

%% @spec is_string(List)-> yes|no|unicode
is_string([]) -> yes;
is_string(List) -> is_string(List, non_unicode).

is_string([C | Rest], non_unicode) when C >= 0, C =< 255 -> is_string(Rest, non_unicode);
is_string([C | Rest], _) when C =< 65000 -> is_string(Rest, unicode);
is_string([], non_unicode) -> yes;
is_string([], unicode) -> unicode;
is_string(_, _) -> no.

list_to_atom2(List) when is_list(List) ->
    case catch (list_to_existing_atom(List)) of
        {'EXIT', _} -> erlang:list_to_atom(List);
        Atom when is_atom(Atom) -> Atom
    end.

%% @doc convert float to string,  f2s(1.5678) -> 1.57
f2s(N) when is_integer(N) ->
    integer_to_list(N) ++ ".00";
f2s(F) when is_float(F) ->
    [A] = io_lib:format("~.2f", [F]),
    A.

%% 截取utf8字符串
substr_utf8(Utf8EncodedString, Length) ->
    substr_utf8(Utf8EncodedString, 1, Length).
substr_utf8(Utf8EncodedString, Start, Length) ->
    ByteLength = 2 * Length,
    Ucs = xmerl_ucs:from_utf8(Utf8EncodedString),
    Utf16Bytes = xmerl_ucs:to_utf16be(Ucs),
    SubStringUtf16 = lists:sublist(Utf16Bytes, Start, ByteLength),
    Ucs1 = xmerl_ucs:from_utf16be(SubStringUtf16),
    xmerl_ucs:to_utf8(Ucs1).

%% 把ip转换成字符串
ip_str({A, B, C, D}) ->
    lists:concat([A, ".", B, ".", C, ".", D]);
ip_str({A, B, C, D, E, F, G, H}) ->
    lists:concat([A, ":", B, ":", C, ":", D, ":", E, ":", F, ":", G, ":", H]);
ip_str(IP) when is_list(IP) ->
    IP;
ip_str(_IP) ->
    [].

%% -----------------------------------------------------------------
%% 确保字符串类型为列表
%% -----------------------------------------------------------------

to_term(BinString) ->
    case bitstring_to_term(BinString) of
        undefined -> [];
        Term -> Term
    end.

%% --------------------------------------------------------
%%                      工具类函数
%% -------------------------------------------------------
%% 产生一个介于Min到Max之间的随机整数
rand(Same, Same) -> Same;
rand(Min, Max) when Max < Min -> 0;
rand(Min, Max) ->
    %% 以保证不同进程都可取得不同的种子
    case get("rand_seed") of
        undefined ->
            random:seed(os:timestamp()),
            put("rand_seed", 1);
        _ -> skip
    end,
    M = Min - 1,
    if
        Max - M =< 0 ->
            0;
        true ->
            random:uniform(Max - M) + M
    end.

%% 向上取整
ceil(N) ->
    T = trunc(N),
    case N == T of
        true -> T;
        false -> 1 + T
    end.

%% 向下取整
floor(X) ->
    T = trunc(X),
    case X < T of
        true -> T - 1;
        _ -> T
    end.

sleep(T) ->
    receive
    after T -> ok
    end.

sleep(T, F) ->
    receive
    after T -> F()
    end.

%% 掷骰子
random_dice(Face, 1) ->
    random(1, Face);
random_dice(Face, Times) ->
    lists:sum(for(1, Times, fun(_) -> random(1, Face) end)).

%% @doc 机率
odds(Numerator, Denominator) ->
    Odds = random:uniform(Denominator),
    if
        Odds =< Numerator ->
            true;
        true ->
            false
    end.
odds_list(List) ->
    Sum = odds_list_sum(List),
    odds_list(List, Sum).
odds_list([{Id, Odds} | List], Sum) ->
    case odds(Odds, Sum) of
        true ->
            Id;
        false ->
            odds_list(List, Sum - Odds)
    end.
odds_list_sum(List) ->
    {_List1, List2} = lists:unzip(List),
    lists:sum(List2).


%% 获取客户端IP
get_ip(Socket) ->
    case inet:peername(Socket) of
        {ok, {Ip, _Port}} ->
            Ip;
        {error, _Reason} ->
            {0, 0, 0, 0}
    end.

%% get IP address string from Socket
ip(Socket) ->
    {Ip0, Ip1, Ip2, Ip3} = get_ip(Socket),
    list_to_binary(integer_to_list(Ip0) ++ "." ++ integer_to_list(Ip1) ++ "." ++ integer_to_list(Ip2) ++ "." ++ integer_to_list(Ip3)).

%% 计数器
counter(Type) ->
    counter(Type, 1).
counter(Type, Inc) ->
    ets:update_counter(ets_counter, Type, Inc).

%% 字符加密
check_char_encrypt(Id, Time, TK) ->
    TICKET = "8YnELt8MmA4jVED8",
    Hex = md5(lists:concat([Time, Id, TICKET])),
%%    NowTime = unixtime(),
%%    Hex =:= TK andalso NowTime - Time < 86400.
    Hex =:= TK.

%% 插入字典
add_dict(Key, Obj, Dict) ->
    case dict:is_key(Key, Dict) of
        true ->
            Dict1 = dict:erase(Key, Dict),
            dict:append(Key, Obj, Dict1);
        false ->
            dict:append(Key, Obj, Dict)
    end.

%%删除字典
del_dict(Key, Dict) ->
    case dict:is_key(Key, Dict) of
        true ->
            dict:erase(Key, Dict);
        false ->
            Dict
    end.

%% --------------------------------------------------
%%                 过滤检测函数
%% --------------------------------------------------
filter_text_gm(Text) when is_bitstring(Text) ->
    Text;
filter_text_gm(Text) when is_list(Text) ->
    list_to_bitstring(Text).

%% 敏感词过滤
%% @param Text list() | bitstring()
%% @return bitstring() 过滤后的文本
filter_text(Text, Lv) when is_bitstring(Text) ->
    S = bitstring_to_list(Text),
    filter_text(S, Lv);
filter_text(Text, Lv) when is_list(Text) ->
    [Term] = io_lib:format("~ts", [Text]),
    mod_word:replace_sensitive_talk(Term, Lv).

%% 名字过滤
%% @param Text list() | bitstring()
%% @return bitstring() 过滤后的文本
filter_name(Text) when is_bitstring(Text) ->
    S = bitstring_to_list(Text),
    filter_name(S);
filter_name(Text) when is_list(Text) ->
    [Term] = io_lib:format("~ts", [Text]),
    mod_word:replace_sensitive_name(Term).

%% 敏感词检测
%% @return true 存在关键词
%%          false 不存在关键词
%% @var Text：字符串
check_keyword(Text) when is_list(Text) ->
    mod_word:word_is_sensitive_name(Text);
check_keyword(_Text) ->
    true.


%% 长度合法性检查
check_length(Item, LenLimit) ->
    check_length(Item, 1, LenLimit).
check_length(Item, MinLen, MaxLen) ->
    case unicode:characters_to_list(list_to_binary(Item)) of
        {_ErrorType, _Encoded, _Rest} ->
            false;
        UnicodeList ->
            Len = string_width(UnicodeList),
            Len =< MaxLen andalso Len >= MinLen
    end.

%% 字符宽度，1汉字=2单位长度，1数字字母=1单位长度
string_width(String) ->
    string_width(String, 0).
string_width([], Len) ->
    Len;
string_width([H | T], Len) ->
    case H > 255 of
        true ->
            string_width(T, Len + 2);
        false ->
            string_width(T, Len + 1)
    end.


%% 字符数，1汉字=1数字字母=1单位长度
char_len(String) ->
    case unicode:characters_to_list(list_to_binary(String)) of
        {_ErrorType, _Encoded, _Rest} ->
            0;
        CharList ->
            string_width(CharList)
    end.
%% 字符宽度，1汉字=1单位长度，1数字字母=1单位长度
char_width(String) ->
    char_width(String, 0).
char_width([], Len) ->
    Len;
char_width([H | T], Len) ->
    case H > 255 of
        true ->
            char_width(T, Len + 1);
        false ->
            char_width(T, Len + 1)
    end.

%% 过滤掉字符串中的特殊字符
filter_string(String, CharList) ->
    case is_list(String) of
        true ->
            filter_string_helper(String, CharList, []);
        false when is_binary(String) ->
            ResultString = filter_string_helper(binary_to_list(String), CharList, []),
            list_to_binary(ResultString);
        false ->
            String
    end.

filter_string_helper([], _CharList, ResultString) ->
    ResultString;
filter_string_helper([H | T], CharList, ResultString) ->
    case lists:member(H, CharList) of
        true -> filter_string_helper(T, CharList, ResultString);
        false -> filter_string_helper(T, CharList, ResultString ++ [H])
    end.

%% 字符长度
string_len(String) ->
    case unicode:characters_to_list(list_to_binary(String)) of
        {_ErrorType, _Encoded, _Rest} ->
            0;
        CharList ->
            string_width(CharList)
    end.

%% @doc 组装 INSERT SQL 语句
make_insert_sql(Table, Proplists) ->
    {ColStr, ValStr} = trans_insert_sql_proplists(Proplists, "", ""),
    lists:concat(["INSERT INTO ", Table, " (", ColStr, ") VALUES (", ValStr, ")"]).
trans_insert_sql_proplists([], ColStr, ValStr) ->
    {string:strip(ColStr, right, $,), string:strip(ValStr, right, $,)};
trans_insert_sql_proplists([{Col, Val} | ValList], ColStr, ValStr) ->
    trans_insert_sql_proplists(ValList, ColStr ++ util:thing_to_list(Col) ++ ",", ValStr ++ util:thing_to_list(Val) ++ ",").


%%计算坐标距离
calc_coord_range(X, Y, X1, Y1) ->
    math:sqrt((math:pow(X - X1, 2) + math:pow(Y - Y1, 2))).

%%以x1 y1为中心 x2,y2的坐标相对方向
%%计算方向 1右 2下 3左 4上 5右上 6右下 7左下 8左上 0 重叠
get_direction(X1, Y1, X2, Y2) ->
    if
        X2 - X1 > 0 andalso Y2 == Y1 ->
            1;%%右
        X2 == X1 andalso Y2 - Y1 > 0 ->
            2;%%下
        X2 - X1 < 0 andalso Y2 == Y1 ->
            3;%%左
        X2 == X1 andalso Y2 - Y1 < 0 ->
            4;%%上
        X2 - X1 > 0 andalso Y2 - Y1 < 0 ->
            5;%%右上
        X2 - X1 > 0 andalso Y2 - Y1 > 0 ->
            6;%%右下
        X2 - X1 < 0 andalso Y2 - Y1 > 0 ->
            7;%%左下
        X2 - X1 < 0 andalso Y2 - Y1 < 0 ->
            8;%%左上
        true ->
            0
    end.

%% 根据位置计算跟踪方向
get_trace_direction(1) -> [-1, 0];
get_trace_direction(2) -> [0, -1];
get_trace_direction(3) -> [1, 0];
get_trace_direction(4) -> [0, 1];
get_trace_direction(5) -> [-1, 1];
get_trace_direction(6) -> [-1, -1];
get_trace_direction(7) -> [1, -1];
get_trace_direction(8) -> [1, 1];
get_trace_direction(_Direction) -> [0, 0].

%% 列表权值概率[{1,radio1},{2,radio2},{3,radio3}...]
list_rand_radio(RadioList) ->
    RadioTotal = lists:sum([R || {_, R} <- RadioList]),
    Rp = util:rand(1, RadioTotal),
    [_RandRatio, _First, NewRule] =
        lists:foldl(fun({Id, R}, [Ratio, First, Result]) ->
            End = First + R,
            if
                Ratio > First andalso Ratio =< End ->
                    [Ratio, End, Id];
                true ->
                    [Ratio, End, Result]
            end
        end, [Rp, 0, 0], RadioList),
    NewRule.

list_rand_radio(RadioList, RadioTotal) ->
%% 	RadioTotal = lists:sum([R||{_,R}<-RadioList]),
    Rp = util:rand(1, RadioTotal),
    [_RandRatio, _First, NewRule] =
        lists:foldl(fun({Id, R}, [Ratio, First, Result]) ->
            End = First + R,
            if
                Ratio > First andalso Ratio =< End ->
                    [Ratio, End, Id];
                true ->
                    [Ratio, End, Result]
            end
        end, [Rp, 0, 0], RadioList),
    NewRule.


%% 取消引用
cancel_ref(RefList) ->
    %% 取消计时器
    lists:foreach(fun(Ref) ->
        case is_reference(Ref) of
            true ->
                erlang:cancel_timer(Ref);
            false ->
                skip
        end
    end, RefList).

%% 根据排序函数获取列表前N项
get_top_n(F, TopN, List) ->
    case length(List) =< 2 * TopN of
        true ->
            lists:sublist(lists:sort(F, List), TopN);
        false ->
            get_top_n_helper(F, TopN, List, [])
    end.

get_top_n_helper(F, TopN, List) ->
    lists:sublist(lists:sort(F, List), TopN).
get_top_n_helper(F, TopN, List, ResultL) ->
    H = util:list_rand(List),
    L_true = [V || V <- List, F(V, H)],
    L1 = L_true ++ ResultL,
    Ll_len = length(L1),
    case Ll_len >= TopN of
        true ->
            case Ll_len =< 2 * TopN of
                true ->
                    get_top_n(F, TopN, L1);
                false ->
                    get_top_n_helper(F, TopN, L_true, ResultL)
            end;
        false ->
            L_false = [V || V <- List, not F(V, H)],
            get_top_n_helper(F, TopN, L_false, L1)
    end.

%% 将一个自然数随机分成N分
random_n(N, Num) ->
    {_, L} =
        lists:foldl(
            fun(Id, {Val, List}) ->
                if
                    Val > 0 ->
                        if
                            Id == Num ->
                                {Val - Val, [Val | List]};
                            true ->
                                V = random_n_val(Val),
                                {Val - V, [V | List]}
                        end;
                    true ->
                        {Val, List}
                end
            end, {N, []}, lists:seq(1, Num)),
    L.
random_n_val(Val) ->
    V = util:rand(1, Val),
    if
        V == Val andalso V /= 1 -> random_n_val(Val);
        true -> V
    end.

%% ID 通过逗号连接起来
combine_id_by_comma(DataList) ->
    combine_id_by_comma(DataList, []).
combine_id_by_comma([], RetData) ->
    string:strip(RetData, right, $,);
combine_id_by_comma([H | T], RetData) ->
    combine_id_by_comma(T, lists:concat([RetData, H, ","])).


%% [[],[]] -> (),(),()格式转换
combine_idlist_by_comma([], RetData) ->
    string:strip(RetData, right, $,);
combine_idlist_by_comma([H | T], RetData) ->
    RetStr = combine_id_by_comma(H, []),
    combine_idlist_by_comma(T, lists:concat([RetData, "(", RetStr, ")", ","])).


%% 用于显示服务器号（专服的服务器号会做扩大处理)
tran_server_id(ServerId) when ServerId > 1000 ->
    round(((ServerId / 1000) - (ServerId div 1000)) * 1000);
tran_server_id(ServerId) ->
    ServerId.

%% 按一定值分割列表（[1,2,3,4,5] -> [[1,2],[3,4],[5]]）
list_split_by_num(List, SplitNum) ->
    list_split_by_num(List, SplitNum, 0, [], []).
list_split_by_num([], _SplitNum, _Num, [], RetList) ->
    RetList;
list_split_by_num([], _SplitNum, _Num, SubList, RetList) ->
    [SubList | RetList];
list_split_by_num([H | List], SplitNum, Num, SubList, RetList) ->
    NewSubList = [H | SubList],
    NewNum = Num + 1,
    if
        NewNum >= SplitNum ->
            list_split_by_num(List, SplitNum, 0, [], [NewSubList | RetList]);
        true ->
            list_split_by_num(List, SplitNum, NewNum, NewSubList, RetList)
    end.

%% @doc lists:nth的逆函数， 设置第n个位置的值。
%% n为0或者超过列表的长度，列表都不会变化.
set_nth(N, Element, List) ->
    set_nth(N, Element, List, 1, []).

set_nth(_N, _Element, [], _Index, Acc) -> lists:reverse(Acc);
set_nth(N, Element, [_ | Rem], N, Acc) ->
    set_nth(N, Element, Rem, N + 1, [Element | Acc]);
set_nth(N, Element, [E | Rem], Index, Acc) ->
    set_nth(N, Element, Rem, Index + 1, [E | Acc]).

%% 根据权重获得一个
%% List = [{K,V}....] V为权重
get_probability_object_from_list(List) ->
    Sum = lists:foldl(fun({_, Pro}, E) ->
        E + Pro
    end, 0, List),
    Rand = util:rand(1, Sum),
    F = fun({Index, Pro2}, E2) ->
        NE2 = E2 + Pro2,
        if NE2 >= Rand ->
            throw({result, Index});
            true ->
                NE2
        end
    end,
    case catch lists:foldl(F, 0, List) of
        {result, R} -> R;
        _ -> 0
    end.

save_file(FileName, BinData) ->
    NewFileName = lists:concat(["./", FileName]),
    case filelib:ensure_dir(NewFileName) of
        ok ->
            {ok, IoDevice} = file:open(NewFileName, write),
            file:write(IoDevice, BinData),
            file:close(IoDevice);
        EnsureDirError ->
            io:format("Save File Error: ~p~n", [EnsureDirError])
    end.

%% @doc 匹配binary，返回匹配到的位置和剩余的bin.
bin(Binary, SubBin) ->
    bin_match(Binary, SubBin, erlang:byte_size(SubBin), 1).

bin_match(Bin, SubBin, Len, Index) when erlang:byte_size(Bin) >= Len ->
    <<GetBin:Len/binary, Rem/binary>> = Bin,
    if
        GetBin =:= SubBin -> {Index, Rem};
        true ->
            <<_I:8, Rem2/binary>> = Bin,
            bin_match(Rem2, SubBin, Len, Index + 1)
    end;
bin_match(Bin, _SubBin, _Len, _Index) -> {0, Bin}.


%% @doc 从二进制中读取include文件名
get_include_file_name(File) ->
    {ok, Binary} = file:read_file(File),
    get_include_file_name2(Binary, []).

get_include_file_name2(Binary, Ret) ->
    case bin(Binary, <<"\n-include(\"">>) of
        {0, _} ->
            Ret;
        {_, Rem} ->
            {EPos, Rem2} = bin(Rem, <<"\").">>),
            ModLen = EPos - 1,
            <<ModName:ModLen/binary, _/binary>> = Rem,
            get_include_file_name2(Rem2, [erlang:binary_to_list(ModName) | Ret])
    end.

hash_string() ->
    lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X>> <= erlang:md5(term_to_binary({node(), make_ref(), os:timestamp()}))]).