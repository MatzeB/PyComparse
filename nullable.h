#pragma once

#if __has_attribute(nonnull)
#define nonnull              _Nonnull
#define nullable             _Nullable
#define ASSUME_NONNULL_BEGIN _Pragma("clang assume_nonnull begin")
#define ASSUME_NONNULL_END   _Pragma("clang assume_nonnull end")
#else
#define nonnull
#define nullable
#define ASSUME_NONNULL_BEGIN
#define ASSUME_NONNULL_END
#endif
