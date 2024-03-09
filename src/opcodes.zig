const std = @import("std");

pub const OpCode = enum(u6) {
    move = 0,
    loadk = 1,
    loadbool = 2,
    loadnil = 3,
    getupval = 4,
    getglobal = 5,
    gettable = 6,
    setglobal = 7,
    setupval = 8,
    settable = 9,
    newtable = 10,
    self = 11,
    add = 12,
    sub = 13,
    mul = 14,
    div = 15,
    mod = 16,
    pow = 17,
    unm = 18,
    not = 19,
    len = 20,
    concat = 21,
    jmp = 22,
    eq = 23,
    lt = 24,
    le = 25,
    @"test" = 26,
    testset = 27,
    call = 28,
    tailcall = 29,
    @"return" = 30,
    forloop = 31,
    forprep = 32,
    tforloop = 33,
    setlist = 34,
    close = 35,
    closure = 36,
    vararg = 37,
    null = 38, // ?

    pub const OpMode = enum {
        iABC,
        iABx,
        iAsBx,
    };

    pub fn InstructionType(op: OpCode) type {
        return switch (op) {
            .move => Instruction.Move,
            .loadk => Instruction.LoadK,
            .loadbool => Instruction.LoadBool,
            .loadnil => Instruction.LoadNil,
            .getupval => Instruction.GetUpVal,
            // .setupval => Instruction.SetUpVal,
            .getglobal => Instruction.GetGlobal,
            .gettable => Instruction.GetTable,
            .setglobal => Instruction.SetGlobal,
            .settable => Instruction.SetTable,
            .newtable => Instruction.NewTable,
            .self => Instruction.Self,
            .add, .sub, .mul, .div, .mod, .pow => Instruction.BinaryMath,
            .unm => Instruction.UnaryMinus,
            .not => Instruction.Not,
            .len => Instruction.Length,
            .concat => Instruction.Concat,
            // .eq, .lt, .le => Instruction.Compare,
            // .test, .testset => Instruction.Test,
            .jmp => Instruction.Jump,
            .call, .tailcall => Instruction.Call,
            .@"return" => Instruction.Return,
            .setlist => Instruction.SetList,
            .vararg => Instruction.VarArg,
        };
    }
    // a maping of Opcode -> OpMode
    const op_modes = blk: {
        const max_fileds = std.math.maxInt(@typeInfo(OpCode).Enum.tag_type);
        var array: [max_fileds]OpMode = undefined;

        for (@typeInfo(OpCode).Enum.fields) |field| {
            const Type = @field(OpCode, field.name).InstructionType();
            const mode: OpMode = switch (@typeInfo(Type).Struct.fields[0].field_type) {
                Instruction.ABC => .iABC,
                Instruction.ABx => .iABx,
                Instruction.AsBx => .iAsBx,
                else => unreachable,
            };
            array[field.value] = mode;
        }
        break :blk array;
    };

    pub fn getOpMode(self: OpCode) OpMode {
        return op_modes[@intFromEnum(self)];
    }

    pub const OpArgMask = enum {
        NotUsed,
        Used,
        RegisterOrJumpOffset,
        ConstantOrRegisterConstant,
    };

    pub const OpMeta = struct {
        b_mode: OpArgMask,
        c_mode: OpArgMask,
        set_register_in_a: bool,
        test_t_mode: bool,
    };

    const op_meta = blk: {
        const max_fields = std.math.maxInt(@typeInfo(OpCode).Enum.tag_type);
        var array: [max_fields]*const OpMeta = undefined;
        for (@typeInfo(OpCode).Enum.fields) |field| {
            const Type = @field(OpCode, field.name).InstructionType();
            const meta = &@field(Type, "meta");
            array[field.value] = meta;
        }
        break :blk array;
    };

    pub fn getBMode(self: OpCode) OpArgMask {
        return op_meta[@intFromEnum(self)].b_mode;
    }

    pub fn getCMode(self: OpCode) OpArgMask {
        return op_meta[@intFromEnum(self)].c_mode;
    }

    pub fn setsRegisterInA(self: OpCode) bool {
        return op_meta[@intFromEnum(self)].sets_register_in_a;
    }

    pub fn testTMode(self: OpCode) bool {
        return op_meta[@intFromEnum(self)].test_t_mode;
    }
};

pub const Instruction = packed struct {
    op: OpCode,
    a: u8,
    fields: u18,

    pub const ABC = packed struct {
        op: OpCode,
        a: u8,
        c: u9,
        b: u9,

        pub fn init(op: OpCode, a: u8, b: u9, c: u9) Instruction.ABC {
            return .{
                .op = op,
                .a = a,
                .b = b,
                .c = c,
            };
        }

        pub const max_a = std.math.maxInt(u8);
        pub const max_b = std.math.maxInt(u9);
        pub const max_c = std.math.maxInt(u9);
    };

    pub const ABx = packed struct {
        op: OpCode,
        a: u8,
        bx: u18,

        pub fn init(op: OpCode, a: u8, bx: u18) Instruction.ABx {
            return .{
                .op = op,
                .a = a,
                .bx = bx,
            };
        }

        pub const max_bx = std.math.maxInt(u18);
    };

    pub const AsBx = packed struct {
        op: OpCode,
        a: u8,
        _bx: u18,

        pub fn init(op: OpCode, a: u8, sbx: i18) Instruction.AsBx {
            return .{
                .op = op,
                .a = a,
                ._bx = signedBxToUnsigned(sbx),
            };
        }

        pub fn getSignedBx(self: *const Instruction.AsBx) i18 {
            return unsignedBxToSigned(self._bx);
        }

        pub fn setSignedBx(self: *Instruction.AsBx, val: i18) void {
            self._bx = signedBxToUnsigned(val);
        }

        pub const max_sbx = std.math.maxInt(i18);
        pub const min_sbx = -max_sbx;

        pub fn unsignedBxToSigned(Bx: u18) i18 {
            _ = Bx;
            // const fitting_int = std.math.IntFittingRange(min_sbx, ABx.max_bx);
            // return @as(i18, @intCast(@as(fitting_int, @intCast(Bx)) - max_sbx));
        }
        pub fn signedBxToUnsigned(sBx: i18) u18 {
            _ = sBx;
            // const fitting_int = std.math.IntFittingRange(min_sbx, ABx.max_bx);
            // return @as(u18, @intCast(fitting_int, @intCast(Bx)) - max_sbx);
        }
    };

    /// R(A) := R(B)
    pub const Move = packed struct {
        instruction: Instruction.ABC,

        pub const meta: OpCode.OpMeta = .{
            .b_mode = .RegisterOrJumpOffset,
            .c_mode = .NotUsed,
            .sets_register_in_a = true,
            .test_t_mode = false,
        };
    };
};

const print = std.debug.print;

pub fn main() void {
    // const move: OpCode = OpCode.move;
    print("move:{}\n", .{@intFromEnum(OpCode.loadk)});
}
