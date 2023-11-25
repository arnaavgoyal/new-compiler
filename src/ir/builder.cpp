#include "ir/builder.h"

Type const *const IRBuilder::bi_void = Type::builtin(token::kw_void);
Type const *const IRBuilder::bi_i8   = Type::builtin(token::kw_i8);
Type const *const IRBuilder::bi_u8   = Type::builtin(token::kw_u8);
Type const *const IRBuilder::bi_i16  = Type::builtin(token::kw_i16);
Type const *const IRBuilder::bi_u16  = Type::builtin(token::kw_u16);
Type const *const IRBuilder::bi_i32  = Type::builtin(token::kw_i32);
Type const *const IRBuilder::bi_u32  = Type::builtin(token::kw_u32);
Type const *const IRBuilder::bi_i64  = Type::builtin(token::kw_i64);
Type const *const IRBuilder::bi_u64  = Type::builtin(token::kw_u64);
Type const *const IRBuilder::bi_f32  = Type::builtin(token::kw_f32);
Type const *const IRBuilder::bi_f64  = Type::builtin(token::kw_f64);
