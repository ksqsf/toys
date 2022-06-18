const std = @import("std");
const c = @cImport({
    @cInclude("libavcodec/avcodec.h");
    @cInclude("libavformat/avformat.h");
});

pub fn main() void {
    var args = std.process.argsAlloc(std.heap.c_allocator) catch |err| {
        std.log.err("cannot allocate memory for args {}", .{err});
        std.os.exit(1);
    };
    defer std.process.argsFree(std.heap.c_allocator, args);

    std.log.info("initializing all the containers, codecs, and protocols", .{});
    var pFormatContext = c.avformat_alloc_context();
    defer c.avformat_close_input(&pFormatContext);

    std.log.info("opening the input file ({s}) and loading format (container) header", .{args[1]});
    var ret = c.avformat_open_input(&pFormatContext, args[1], null, null);
    if (ret != 0) {
        std.log.err("could not open file {s}", .{args[1]});
        std.os.exit(1);
    }
    std.log.info("format {s}, duration {}us, bit_rate {}", .{ pFormatContext.?.*.iformat.?.*.name.?, pFormatContext.?.*.duration, pFormatContext.?.*.bit_rate });

    std.log.info("finding stream info from format", .{});
    if (c.avformat_find_stream_info(pFormatContext, null) < 0) {
        std.log.err("could not get the stream info", .{});
        std.os.exit(1);
    }

    var pCodec: [*c]const c.AVCodec = null;
    var pCodecParams: [*c]c.AVCodecParameters = null;
    var video_stream_index: ?usize = null;
    var i: usize = 0;
    while (i < pFormatContext.?.*.nb_streams) : (i += 1) {
        std.log.info("AVStream->time_base before open coded {}/{}", .{ pFormatContext.?.*.streams[i].?.*.time_base.num, pFormatContext.?.*.streams[i].?.*.time_base.den });
        std.log.info("AVStream->r_frame_rate before open coded {}/{}", .{ pFormatContext.?.*.streams[i].?.*.r_frame_rate.num, pFormatContext.?.*.streams[i].?.*.r_frame_rate.den });
        std.log.info("AVStream->start_time {}", .{pFormatContext.?.*.streams[i].?.*.start_time});
        std.log.info("AVStream->duration {}", .{pFormatContext.?.*.streams[i].?.*.duration});
        var pLocalCodecParams = pFormatContext.?.*.streams[i].?.*.codecpar;
        var pLocalCodec: [*c]const c.AVCodec = c.avcodec_find_decoder(pLocalCodecParams.?.*.codec_id);
        if (pLocalCodec == null) {
            std.log.err("unsupported codec", .{});
            continue;
        }

        if (pLocalCodecParams.?.*.codec_type == c.AVMEDIA_TYPE_VIDEO) {
            if (video_stream_index) |_| {} else {
                video_stream_index = i;
                pCodec = pLocalCodec;
                pCodecParams = pLocalCodecParams;
            }
            std.log.info("Video Codec: resolution {}x{}", .{ pLocalCodecParams.?.*.width, pLocalCodecParams.?.*.height });
        } else if (pLocalCodecParams.?.*.codec_type == c.AVMEDIA_TYPE_AUDIO) {
            std.log.info("Audio Codec: {} channels, sample rate {}", .{ pLocalCodecParams.?.*.channels, pLocalCodecParams.?.*.sample_rate });
        }
        std.log.info("\tCodec {s} ID {} bit_rate {}", .{ pLocalCodec.?.*.name, pLocalCodec.?.*.id, pLocalCodecParams.?.*.bit_rate });
    }

    if (video_stream_index) |_| {} else {
        std.log.err("file {s} does not contain a video stream!", .{args[1]});
        std.os.exit(1);
    }

    var pCodecContext = c.avcodec_alloc_context3(pCodec);
    if (pCodecContext == null) {
        std.log.err("failed to allocate memory for AVCodecContext", .{});
        std.os.exit(1);
    }

    if (c.avcodec_parameters_to_context(pCodecContext, pCodecParams) < 0) {
        std.log.err("failed to copy codec params to codec context", .{});
        std.os.exit(1);
    }

    if (c.avcodec_open2(pCodecContext, pCodec, null) < 0) {
        std.log.err("failed to open codec through avcodec_open2", .{});
        std.os.exit(1);
    }

    var pFrame = c.av_frame_alloc();
    if (pFrame == null) {
        std.log.err("failed to allocate memory for AVFrame", .{});
        std.os.exit(1);
    }
    defer c.av_frame_free(&pFrame);

    var pPacket = c.av_packet_alloc();
    if (pPacket == null) {
        std.log.err("failed to allocate memory for AVPacket", .{});
        std.os.exit(1);
    }
    defer c.av_packet_free(&pPacket);

    var response: i32 = 0;
    var how_many_packets_to_process: i32 = 8;

    while (c.av_read_frame(pFormatContext, pPacket) >= 0) {
        if (pPacket.?.*.stream_index == video_stream_index.?) {
            std.log.info("AVPacket->pts {}", .{pPacket.?.*.pts});
            response = decode_packet(pPacket, pCodecContext, pFrame);
            if (response < 0)
                break;
            how_many_packets_to_process -= 1;
            if (how_many_packets_to_process <= 0)
                break;
        }
        c.av_packet_unref(pPacket);
    }
}

fn decode_packet(pPacket: *c.AVPacket, pCodecContext: *c.AVCodecContext, pFrame: *c.AVFrame) i32 {
    var response: i32 = c.avcodec_send_packet(pCodecContext, pPacket);
    var errbuf: [c.AV_ERROR_MAX_STRING_SIZE]u8 = undefined;

    if (response < 0) {
        std.log.err("error while sending a packet to the decoder: {s}", .{av_err2str(&errbuf, response)});
        return response;
    }

    while (response >= 0) {
        response = c.avcodec_receive_frame(pCodecContext, pFrame);
        if (response == c.AVERROR(c.EAGAIN) or response == c.AVERROR_EOF) {
            break;
        } else if (response < 0) {
            std.log.err("error while receiving a frame from the decoder: {s}", .{av_err2str(&errbuf, response)});
            return response;
        }

        if (response >= 0) {
            std.log.info("Frame {} (type={}, size={} bytes, format={}) pts {} key_frame {} [DTS {}]", .{ pCodecContext.*.frame_number, c.av_get_picture_type_char(pFrame.*.pict_type), pFrame.*.pkt_size, pFrame.*.format, pFrame.*.pts, pFrame.*.key_frame, pFrame.*.coded_picture_number });

            var filename_buf: [1024]u8 = undefined;
            var filename = std.fmt.bufPrint(&filename_buf, "{s}-{}.pgm", .{ "frame", pCodecContext.frame_number }) catch {
                std.log.warn("failed to format the filename, skipping", .{});
                continue;
            };
            if (pFrame.format != c.AV_PIX_FMT_YUV420P) {
                std.log.warn("the generated file may not be a greyscale image, but could e.g. be just the R component if the video format is RGB", .{});
            }
            save_gray_frame(pFrame.data[0], @intCast(usize, pFrame.linesize[0]), @intCast(usize, pFrame.width), @intCast(usize, pFrame.height), filename);
        }
    }
    return 0;
}

fn save_gray_frame(buf: [*]u8, wrap: usize, xsize: usize, ysize: usize, filename: []const u8) void {
    var cwd = std.fs.cwd();
    var file = cwd.createFile(filename, .{ .read = true, .truncate = true, .exclusive = false, .lock = .None }) catch |err| {
        std.log.err("failed to open file: {}", .{err});
        return;
    };
    defer file.close();
    var writer = file.writer();
    _ = writer.print("P5\n{} {}\n{}\n", .{ xsize, ysize, 255 }) catch |err| {
        std.log.err("failed to write file: {}", .{err});
        return;
    };
    var i: usize = 0;
    while (i < ysize) : (i += 1) {
        _ = writer.writeAll(buf[i * wrap .. i * wrap + xsize]) catch |err| {
            std.log.err("failed to write file: {}", .{err});
            return;
        };
    }
}

// was a macro: av_make_error_string((char[AV_ERROR_MAX_STRING_SIZE]){0}, AV_ERROR_MAX_STRING_SIZE, errnum)
//
// static inline char *av_make_error_string(char *errbuf, size_t errbuf_size, int errnum)
// {
//     av_strerror(errnum, errbuf, errbuf_size);
//     return errbuf;
// }
fn av_err2str(errbuf: []u8, errnum: i32) []u8 {
    _ = c.av_strerror(errnum, @ptrCast([*c]u8, errbuf), errbuf.len);
    return errbuf;
}
