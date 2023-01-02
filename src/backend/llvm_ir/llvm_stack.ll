; ModuleID = 'stack.c'
source_filename = "stack.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.Stack_t = type { i8*, i8*, i8* }

@stack_buffer = global [10000 x i8] zeroinitializer, align 16
@stack = global %struct.Stack_t { i8* getelementptr inbounds ([10000 x i8], [10000 x i8]* @stack_buffer, i32 0, i32 0), i8* getelementptr inbounds ([10000 x i8], [10000 x i8]* @stack_buffer, i32 0, i32 0), i8* getelementptr (i8, i8* getelementptr inbounds ([10000 x i8], [10000 x i8]* @stack_buffer, i32 0, i32 0), i64 10000) }, align 8
@.str = private unnamed_addr constant [46 x i8] c"Runtime Error: Cannot push -- stack is full.\0A\00", align 1
@.str.1 = private unnamed_addr constant [61 x i8] c"Runtime Error: cannot pop %ld bytes -- stack has %ld bytes.\0A\00", align 1
@.str.2 = private unnamed_addr constant [15 x i8] c"Popped: `%ld`\0A\00", align 1
@.str.3 = private unnamed_addr constant [14 x i8] c"Popped: `%c`\0A\00", align 1

; Function Attrs: noinline nounwind optnone uwtable
define void @Stack_check_overflow(%struct.Stack_t*, i64) #0 {
  %3 = alloca %struct.Stack_t*, align 8
  %4 = alloca i64, align 8
  store %struct.Stack_t* %0, %struct.Stack_t** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  %6 = getelementptr inbounds %struct.Stack_t, %struct.Stack_t* %5, i32 0, i32 0
  %7 = load i8*, i8** %6, align 8
  %8 = load i64, i64* %4, align 8
  %9 = getelementptr inbounds i8, i8* %7, i64 %8
  %10 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  %11 = getelementptr inbounds %struct.Stack_t, %struct.Stack_t* %10, i32 0, i32 2
  %12 = load i8*, i8** %11, align 8
  %13 = icmp uge i8* %9, %12
  br i1 %13, label %14, label %16

; <label>:14:                                     ; preds = %2
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str, i32 0, i32 0))
  call void @exit(i32 1) #4
  unreachable

; <label>:16:                                     ; preds = %2
  ret void
}

declare i32 @printf(i8*, ...) #1

; Function Attrs: noreturn nounwind
declare void @exit(i32) #2

; Function Attrs: noinline nounwind optnone uwtable
define void @Stack_check_underflow(%struct.Stack_t*, i64) #0 {
  %3 = alloca %struct.Stack_t*, align 8
  %4 = alloca i64, align 8
  store %struct.Stack_t* %0, %struct.Stack_t** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  %6 = getelementptr inbounds %struct.Stack_t, %struct.Stack_t* %5, i32 0, i32 0
  %7 = load i8*, i8** %6, align 8
  %8 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  %9 = getelementptr inbounds %struct.Stack_t, %struct.Stack_t* %8, i32 0, i32 1
  %10 = load i8*, i8** %9, align 8
  %11 = ptrtoint i8* %7 to i64
  %12 = ptrtoint i8* %10 to i64
  %13 = sub i64 %11, %12
  %14 = load i64, i64* %4, align 8
  %15 = icmp ult i64 %13, %14
  br i1 %15, label %16, label %28

; <label>:16:                                     ; preds = %2
  %17 = load i64, i64* %4, align 8
  %18 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  %19 = getelementptr inbounds %struct.Stack_t, %struct.Stack_t* %18, i32 0, i32 0
  %20 = load i8*, i8** %19, align 8
  %21 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  %22 = getelementptr inbounds %struct.Stack_t, %struct.Stack_t* %21, i32 0, i32 1
  %23 = load i8*, i8** %22, align 8
  %24 = ptrtoint i8* %20 to i64
  %25 = ptrtoint i8* %23 to i64
  %26 = sub i64 %24, %25
  %27 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([61 x i8], [61 x i8]* @.str.1, i32 0, i32 0), i64 %17, i64 %26)
  call void @exit(i32 1) #4
  unreachable

; <label>:28:                                     ; preds = %2
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define void @Stack_push_u8(%struct.Stack_t*, i8 zeroext) #0 {
  %3 = alloca %struct.Stack_t*, align 8
  %4 = alloca i8, align 1
  store %struct.Stack_t* %0, %struct.Stack_t** %3, align 8
  store i8 %1, i8* %4, align 1
  %5 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  call void @Stack_check_overflow(%struct.Stack_t* %5, i64 1)
  %6 = load i8, i8* %4, align 1
  %7 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  %8 = getelementptr inbounds %struct.Stack_t, %struct.Stack_t* %7, i32 0, i32 0
  %9 = load i8*, i8** %8, align 8
  store i8 %6, i8* %9, align 1
  %10 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  %11 = getelementptr inbounds %struct.Stack_t, %struct.Stack_t* %10, i32 0, i32 0
  %12 = load i8*, i8** %11, align 8
  %13 = getelementptr inbounds i8, i8* %12, i64 1
  store i8* %13, i8** %11, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define void @Stack_push_u64(%struct.Stack_t*, i64) #0 {
  %3 = alloca %struct.Stack_t*, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64*, align 8
  store %struct.Stack_t* %0, %struct.Stack_t** %3, align 8
  store i64 %1, i64* %4, align 8
  %6 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  call void @Stack_check_overflow(%struct.Stack_t* %6, i64 8)
  %7 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  %8 = getelementptr inbounds %struct.Stack_t, %struct.Stack_t* %7, i32 0, i32 0
  %9 = load i8*, i8** %8, align 8
  %10 = bitcast i8* %9 to i64*
  store i64* %10, i64** %5, align 8
  %11 = load i64, i64* %4, align 8
  %12 = load i64*, i64** %5, align 8
  store i64 %11, i64* %12, align 8
  %13 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  %14 = getelementptr inbounds %struct.Stack_t, %struct.Stack_t* %13, i32 0, i32 0
  %15 = load i8*, i8** %14, align 8
  %16 = getelementptr inbounds i8, i8* %15, i64 8
  store i8* %16, i8** %14, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define void @Stack_push_bytes(%struct.Stack_t*, i8*, i64) #0 {
  %4 = alloca %struct.Stack_t*, align 8
  %5 = alloca i8*, align 8
  %6 = alloca i64, align 8
  store %struct.Stack_t* %0, %struct.Stack_t** %4, align 8
  store i8* %1, i8** %5, align 8
  store i64 %2, i64* %6, align 8
  %7 = load %struct.Stack_t*, %struct.Stack_t** %4, align 8
  %8 = load i64, i64* %6, align 8
  %9 = mul i64 1, %8
  call void @Stack_check_overflow(%struct.Stack_t* %7, i64 %9)
  %10 = load %struct.Stack_t*, %struct.Stack_t** %4, align 8
  %11 = getelementptr inbounds %struct.Stack_t, %struct.Stack_t* %10, i32 0, i32 0
  %12 = load i8*, i8** %11, align 8
  %13 = load i8*, i8** %5, align 8
  %14 = load i64, i64* %6, align 8
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %12, i8* %13, i64 %14, i32 1, i1 false)
  %15 = load i64, i64* %6, align 8
  %16 = mul i64 1, %15
  %17 = load %struct.Stack_t*, %struct.Stack_t** %4, align 8
  %18 = getelementptr inbounds %struct.Stack_t, %struct.Stack_t* %17, i32 0, i32 0
  %19 = load i8*, i8** %18, align 8
  %20 = getelementptr inbounds i8, i8* %19, i64 %16
  store i8* %20, i8** %18, align 8
  ret void
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i32, i1) #3

; Function Attrs: noinline nounwind optnone uwtable
define void @Stack_reserve(%struct.Stack_t*, i64) #0 {
  %3 = alloca %struct.Stack_t*, align 8
  %4 = alloca i64, align 8
  store %struct.Stack_t* %0, %struct.Stack_t** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  %6 = load i64, i64* %4, align 8
  %7 = mul i64 1, %6
  call void @Stack_check_overflow(%struct.Stack_t* %5, i64 %7)
  %8 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  %9 = getelementptr inbounds %struct.Stack_t, %struct.Stack_t* %8, i32 0, i32 0
  %10 = load i8*, i8** %9, align 8
  %11 = load i64, i64* %4, align 8
  call void @llvm.memset.p0i8.i64(i8* %10, i8 0, i64 %11, i32 1, i1 false)
  %12 = load i64, i64* %4, align 8
  %13 = mul i64 1, %12
  %14 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  %15 = getelementptr inbounds %struct.Stack_t, %struct.Stack_t* %14, i32 0, i32 0
  %16 = load i8*, i8** %15, align 8
  %17 = getelementptr inbounds i8, i8* %16, i64 %13
  store i8* %17, i8** %15, align 8
  ret void
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i32, i1) #3

; Function Attrs: noinline nounwind optnone uwtable
define i8* @Stack_pop(%struct.Stack_t*, i64) #0 {
  %3 = alloca %struct.Stack_t*, align 8
  %4 = alloca i64, align 8
  store %struct.Stack_t* %0, %struct.Stack_t** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  %6 = load i64, i64* %4, align 8
  call void @Stack_check_underflow(%struct.Stack_t* %5, i64 %6)
  %7 = load i64, i64* %4, align 8
  %8 = mul i64 %7, 1
  %9 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  %10 = getelementptr inbounds %struct.Stack_t, %struct.Stack_t* %9, i32 0, i32 0
  %11 = load i8*, i8** %10, align 8
  %12 = sub i64 0, %8
  %13 = getelementptr inbounds i8, i8* %11, i64 %12
  store i8* %13, i8** %10, align 8
  %14 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  %15 = getelementptr inbounds %struct.Stack_t, %struct.Stack_t* %14, i32 0, i32 0
  %16 = load i8*, i8** %15, align 8
  ret i8* %16
}

; Function Attrs: noinline nounwind optnone uwtable
define i8* @Stack_offset(%struct.Stack_t*, i64) #0 {
  %3 = alloca %struct.Stack_t*, align 8
  %4 = alloca i64, align 8
  store %struct.Stack_t* %0, %struct.Stack_t** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  %6 = load i64, i64* %4, align 8
  call void @Stack_check_overflow(%struct.Stack_t* %5, i64 %6)
  %7 = load %struct.Stack_t*, %struct.Stack_t** %3, align 8
  %8 = getelementptr inbounds %struct.Stack_t, %struct.Stack_t* %7, i32 0, i32 0
  %9 = load i8*, i8** %8, align 8
  %10 = load i64, i64* %4, align 8
  %11 = mul i64 1, %10
  %12 = sub i64 0, %11
  %13 = getelementptr inbounds i8, i8* %9, i64 %12
  ret i8* %13
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i64*, align 8
  %3 = alloca i8*, align 8
  store i32 0, i32* %1, align 4
  call void @Stack_push_u8(%struct.Stack_t* @stack, i8 zeroext 99)
  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 12345)
  %4 = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)
  %5 = bitcast i8* %4 to i64*
  store i64* %5, i64** %2, align 8
  %6 = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 1)
  store i8* %6, i8** %3, align 8
  %7 = load i64*, i64** %2, align 8
  %8 = load i64, i64* %7, align 8
  %9 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.2, i32 0, i32 0), i64 %8)
  %10 = load i8*, i8** %3, align 8
  %11 = load i8, i8* %10, align 1
  %12 = sext i8 %11 to i32
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.3, i32 0, i32 0), i32 %12)
  ret i32 0
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { noreturn nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { argmemonly nounwind }
attributes #4 = { noreturn nounwind }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 6.0.0-1ubuntu2 (tags/RELEASE_600/final)"}
