-module(http).
-export([parse_request/1, ok/1, get/1]).
%-export([parse_request/1, request_line/1, headers/1, message_body/1, http_version/1, header/1]).

parse_request(RO) -> 
    {Request, R1} = request_line(RO),
    {Headers, R2} = headers(R1),
    {Body, _} = message_body(R2),
    {Request, Headers, Body}. 


request_line([$G, $E, $T, 32 |RO]) -> 
    {URI, R1} = request_uri(RO),
    {Ver, R2} = http_version(R1),
    [13, 10|R3] = R2,
    {{get, URI, Ver}, R3}.

request_uri([32|RO]) ->
    {[], RO};
request_uri([C|RO]) ->
    {Rest, R1} = request_uri(RO),
    {[C|Rest], R1}.

http_version([$H, $T, $T, $P, $/, $1, $., $1 | RO]) -> 
    {v11, RO};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | RO]) ->
    {v10, RO}.

headers([13, 10|RO]) -> 
    {[], RO};
headers(RO) ->
    {Header, R1} = header(RO),
    {Rest, R2} = headers(R1),
    {[Header|Rest], R2}.

header([13, 10|RO]) -> 
    {[], RO};
header([C|RO]) ->
    {Rest, R1} = header(RO),
    {[C|Rest], R1}.


message_body(R) ->
    {R, []}.

ok(Body) -> 
    "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

get(URI) ->
    "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".