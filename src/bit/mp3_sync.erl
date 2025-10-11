-module(mp3_sync).
-export([find_sync/2]).


find_sync(Bin, N) ->
    case is_header(N, Bin) of
        {ok, Len1, _} ->
            case is_header(N + Len1, Bin) of
                {ok, Len2, _} ->
                    case is_header(N + Len1 + Len2, Bin) of
                        {ok, _, _} ->
                            {ok, N};
                        error ->
                            find_sync(Bin, N+1)
                    end;
                error ->
                    find_sync(Bin, N+1)
            end;
        error ->
            find_sync(Bin, N+1)
    end.

is_header(N, Bin) ->
    unpack_header(get_word(N,Bin)). 

unpack_header(X) ->
    try decode_header(X)
    catch
        _:_ -> error
    end.

get_word(N, Bin) ->
    {_, <<C:4/binary,_/binary>>} = split_binary(Bin, N),
    C.

decode_header(<<2#11111111111:11,
                B:2, C:2, D:1, E:4, F:2, G:1, Bits:9>>) ->
    Vsn = case B of
        0 -> {2,5};
        1 -> exit(badVsn);
        2 -> 2;
        3 -> 1
    end,
    Layer = case C of
        0 -> exit(badLayer);
        1 -> 3;
        2 -> 2;
        3 -> 1
    end,
    %% Protection = D,
    BitRate    = bitrate(Vsn, Layer, E) * 1000,
    SampleRate = samplerate(Vsn, F),
    Padding    = G,
    FrameLength = framelength(Layer, BitRate, SampleRate, Padding),
    if
        FrameLength < 21 ->
            exit(frameSize);
        true ->
            {ok, FrameLength, {Layer, BitRate, SampleRate, Vsn, Bits}}
    end;
decode_header(_) ->
    exit(badHeader).


% 补充课本第七章关于视频解析的代码的补充，否则无法运行
bitrate(Vsn, Layer, BitRateIndex) ->
    BitRates =
        case {Vsn, Layer} of
            {1, 1} -> [0,32,64,96,128,160,192,224,256,288,320,352,384,416,448];
            {1, 2} -> [0,32,48,56,64,80,96,112,128,160,192,224,256,320,384];
            {1, 3} -> [0,32,40,48,56,64,80,96,112,128,160,192,224,256,320];
            {2, _} -> [0,8,16,24,32,40,48,56,64,80,96,112,128,144,160];
            {{2,5}, _} -> [0,8,16,24,32,40,48,56,64,80,96,112,128,144,160]
        end,
    lists:nth(BitRateIndex + 1, BitRates).

samplerate(Vsn, SampleRateIndex) ->
    Rates =
        case Vsn of
            1 -> [44100, 48000, 32000];
            2 -> [22050, 24000, 16000];
            {2,5} -> [11025, 12000, 8000]
        end,
    lists:nth(SampleRateIndex + 1, Rates).

framelength(Layer, BitRate, SampleRate, Padding) ->
    case Layer of
        1 -> (12 * BitRate div SampleRate + Padding) * 4;
        2 -> 144 * BitRate div SampleRate + Padding;
        3 -> 72 * BitRate div SampleRate + Padding
    end.
