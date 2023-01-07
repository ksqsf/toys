; ModuleID = 'rcu.cpp'
source_filename = "rcu.cpp"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx11.0.0"

%"class.std::__1::thread" = type { %struct._opaque_pthread_t* }
%struct._opaque_pthread_t = type { i64, %struct.__darwin_pthread_handler_rec*, [8176 x i8] }
%struct.__darwin_pthread_handler_rec = type { void (i8*)*, i8*, %struct.__darwin_pthread_handler_rec* }
%"class.std::__1::__thread_struct" = type { %"class.std::__1::__thread_struct_imp"* }
%"class.std::__1::__thread_struct_imp" = type opaque
%struct._opaque_pthread_attr_t = type { i64, [56 x i8] }
%"class.std::__1::vector" = type { %"class.std::__1::__vector_base" }
%"class.std::__1::__vector_base" = type { %"class.std::__1::thread"*, %"class.std::__1::thread"*, %"class.std::__1::__compressed_pair" }
%"class.std::__1::__compressed_pair" = type { %"struct.std::__1::__compressed_pair_elem" }
%"struct.std::__1::__compressed_pair_elem" = type { %"class.std::__1::thread"* }
%"class.std::__1::__thread_specific_ptr" = type { i64 }
%"class.std::__1::__vector_base_common" = type { i8 }
%"class.std::length_error" = type { %"class.std::logic_error" }
%"class.std::logic_error" = type { %"class.std::exception", %"class.std::__1::__libcpp_refstring" }
%"class.std::exception" = type { i32 (...)** }
%"class.std::__1::__libcpp_refstring" = type { i8* }

@n_threads = global { { i32 } } zeroinitializer, align 4
@tid = internal thread_local global i32 0, align 4
@.str = private unnamed_addr constant [26 x i8] c"thread constructor failed\00", align 1
@.str.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@.str.2 = private unnamed_addr constant [68 x i8] c"allocator<T>::allocate(size_t n) 'n' exceeds maximum supported size\00", align 1
@_ZTISt12length_error = external constant i8*
@_ZTVSt12length_error = external unnamed_addr constant { [5 x i8*] }, align 8

; Function Attrs: norecurse nounwind readnone ssp uwtable
define void @_Z13rcu_read_lockv() local_unnamed_addr #0 {
  ret void
}

; Function Attrs: norecurse nounwind readnone ssp uwtable
define void @_Z15rcu_read_unlockv() local_unnamed_addr #0 {
  ret void
}

; Function Attrs: norecurse nounwind readnone ssp uwtable
define void @_Z8rcu_syncv() local_unnamed_addr #0 {
  ret void
}

; Function Attrs: ssp uwtable
define void @_Z5spawnv(%"class.std::__1::thread"* noalias sret align 8 %0) local_unnamed_addr #1 personality i32 (...)* @__gxx_personality_v0 {
  %2 = tail call noalias nonnull dereferenceable(8) i8* @_Znwm(i64 8) #14
  %3 = bitcast i8* %2 to %"class.std::__1::__thread_struct"*
  invoke void @_ZNSt3__115__thread_structC1Ev(%"class.std::__1::__thread_struct"* nonnull %3)
          to label %4 unwind label %13

4:                                                ; preds = %1
  %5 = invoke noalias nonnull dereferenceable(8) i8* @_Znwm(i64 8) #14
          to label %6 unwind label %28

6:                                                ; preds = %4
  %7 = ptrtoint i8* %2 to i64
  %8 = bitcast i8* %5 to i64*
  store i64 %7, i64* %8, align 8, !tbaa !3
  %9 = getelementptr inbounds %"class.std::__1::thread", %"class.std::__1::thread"* %0, i64 0, i32 0
  %10 = invoke i32 @pthread_create(%struct._opaque_pthread_t** %9, %struct._opaque_pthread_attr_t* null, i8* (i8*)* nonnull @"_ZNSt3__114__thread_proxyINS_5tupleIJNS_10unique_ptrINS_15__thread_structENS_14default_deleteIS3_EEEEZ5spawnvE3$_0EEEEEPvS9_", i8* nonnull %5)
          to label %11 unwind label %17

11:                                               ; preds = %6
  %12 = icmp eq i32 %10, 0
  br i1 %12, label %38, label %26

13:                                               ; preds = %1
  %14 = landingpad { i8*, i32 }
          cleanup
  %15 = extractvalue { i8*, i32 } %14, 0
  %16 = extractvalue { i8*, i32 } %14, 1
  br label %32

17:                                               ; preds = %26, %6
  %18 = landingpad { i8*, i32 }
          cleanup
  %19 = extractvalue { i8*, i32 } %18, 0
  %20 = extractvalue { i8*, i32 } %18, 1
  %21 = bitcast i8* %5 to %"class.std::__1::__thread_struct"**
  %22 = load %"class.std::__1::__thread_struct"*, %"class.std::__1::__thread_struct"** %21, align 8, !tbaa !8
  store %"class.std::__1::__thread_struct"* null, %"class.std::__1::__thread_struct"** %21, align 8, !tbaa !8
  %23 = icmp eq %"class.std::__1::__thread_struct"* %22, null
  br i1 %23, label %32, label %24

24:                                               ; preds = %17
  tail call void @_ZNSt3__115__thread_structD1Ev(%"class.std::__1::__thread_struct"* nonnull %22) #15
  %25 = bitcast %"class.std::__1::__thread_struct"* %22 to i8*
  tail call void @_ZdlPv(i8* %25) #16
  br label %32

26:                                               ; preds = %11
  invoke void @_ZNSt3__120__throw_system_errorEiPKc(i32 %10, i8* getelementptr inbounds ([26 x i8], [26 x i8]* @.str, i64 0, i64 0)) #17
          to label %27 unwind label %17

27:                                               ; preds = %26
  unreachable

28:                                               ; preds = %4
  %29 = landingpad { i8*, i32 }
          cleanup
  %30 = extractvalue { i8*, i32 } %29, 0
  %31 = extractvalue { i8*, i32 } %29, 1
  tail call void @_ZNSt3__115__thread_structD1Ev(%"class.std::__1::__thread_struct"* nonnull %3) #15
  br label %32

32:                                               ; preds = %17, %24, %28, %13
  %33 = phi i8* [ %2, %28 ], [ %2, %13 ], [ %5, %24 ], [ %5, %17 ]
  %34 = phi i32 [ %31, %28 ], [ %16, %13 ], [ %20, %24 ], [ %20, %17 ]
  %35 = phi i8* [ %30, %28 ], [ %15, %13 ], [ %19, %24 ], [ %19, %17 ]
  tail call void @_ZdlPv(i8* nonnull %33) #16
  %36 = insertvalue { i8*, i32 } undef, i8* %35, 0
  %37 = insertvalue { i8*, i32 } %36, i32 %34, 1
  resume { i8*, i32 } %37

38:                                               ; preds = %11
  ret void
}

; Function Attrs: argmemonly nounwind willreturn
declare void @llvm.lifetime.start.p0i8(i64 immarg, i8* nocapture) #2

; Function Attrs: argmemonly nounwind willreturn
declare void @llvm.lifetime.end.p0i8(i64 immarg, i8* nocapture) #2

; Function Attrs: norecurse ssp uwtable
define i32 @main() local_unnamed_addr #3 personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) {
  %1 = alloca %"class.std::__1::vector", align 8
  %2 = alloca %"class.std::__1::thread", align 8
  %3 = bitcast %"class.std::__1::vector"* %1 to i8*
  call void @llvm.lifetime.start.p0i8(i64 24, i8* nonnull %3) #15
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 dereferenceable(24) %3, i8 0, i64 24, i1 false) #15
  %4 = bitcast %"class.std::__1::thread"* %2 to i8*
  %5 = getelementptr inbounds %"class.std::__1::vector", %"class.std::__1::vector"* %1, i64 0, i32 0, i32 1
  %6 = getelementptr inbounds %"class.std::__1::vector", %"class.std::__1::vector"* %1, i64 0, i32 0, i32 2, i32 0, i32 0
  %7 = getelementptr inbounds %"class.std::__1::thread", %"class.std::__1::thread"* %2, i64 0, i32 0
  %8 = bitcast %"class.std::__1::thread"* %2 to i64*
  %9 = bitcast %"class.std::__1::thread"** %5 to i64*
  call void @llvm.lifetime.start.p0i8(i64 8, i8* nonnull %4) #15
  invoke void @_Z5spawnv(%"class.std::__1::thread"* nonnull sret align 8 %2)
          to label %10 unwind label %12

10:                                               ; preds = %0
  invoke void @_ZNSt3__16vectorINS_6threadENS_9allocatorIS1_EEE21__push_back_slow_pathIS1_EEvOT_(%"class.std::__1::vector"* nonnull %1, %"class.std::__1::thread"* nonnull align 8 dereferenceable(8) %2)
          to label %11 unwind label %16

11:                                               ; preds = %10
  call void @_ZNSt3__16threadD1Ev(%"class.std::__1::thread"* nonnull %2) #15
  call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %4) #15
  call void @llvm.lifetime.start.p0i8(i64 8, i8* nonnull %4) #15
  invoke void @_Z5spawnv(%"class.std::__1::thread"* nonnull sret align 8 %2)
          to label %73 unwind label %12

12:                                               ; preds = %94, %83, %11, %0
  %13 = landingpad { i8*, i32 }
          cleanup
  %14 = extractvalue { i8*, i32 } %13, 0
  %15 = extractvalue { i8*, i32 } %13, 1
  br label %20

16:                                               ; preds = %99, %88, %77, %10
  %17 = landingpad { i8*, i32 }
          cleanup
  %18 = extractvalue { i8*, i32 } %17, 0
  %19 = extractvalue { i8*, i32 } %17, 1
  call void @_ZNSt3__16threadD1Ev(%"class.std::__1::thread"* nonnull %2) #15
  br label %20

20:                                               ; preds = %16, %12
  %21 = phi i32 [ %19, %16 ], [ %15, %12 ]
  %22 = phi i8* [ %18, %16 ], [ %14, %12 ]
  call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %4) #15
  br label %51

23:                                               ; preds = %44
  %24 = load %"class.std::__1::thread"*, %"class.std::__1::thread"** %106, align 8, !tbaa !9
  br label %25

25:                                               ; preds = %23, %105
  %26 = phi %"class.std::__1::thread"* [ %24, %23 ], [ %107, %105 ]
  %27 = icmp eq %"class.std::__1::thread"* %26, null
  br i1 %27, label %41, label %28

28:                                               ; preds = %25
  %29 = bitcast %"class.std::__1::thread"* %26 to i8*
  %30 = load %"class.std::__1::thread"*, %"class.std::__1::thread"** %5, align 8, !tbaa !12
  %31 = icmp eq %"class.std::__1::thread"* %30, %26
  br i1 %31, label %39, label %32

32:                                               ; preds = %28, %32
  %33 = phi %"class.std::__1::thread"* [ %34, %32 ], [ %30, %28 ]
  %34 = getelementptr inbounds %"class.std::__1::thread", %"class.std::__1::thread"* %33, i64 -1
  call void @_ZNSt3__16threadD1Ev(%"class.std::__1::thread"* nonnull %34) #15
  %35 = icmp eq %"class.std::__1::thread"* %34, %26
  br i1 %35, label %36, label %32

36:                                               ; preds = %32
  %37 = bitcast %"class.std::__1::vector"* %1 to i8**
  %38 = load i8*, i8** %37, align 8, !tbaa !9
  br label %39

39:                                               ; preds = %36, %28
  %40 = phi i8* [ %38, %36 ], [ %29, %28 ]
  store %"class.std::__1::thread"* %26, %"class.std::__1::thread"** %5, align 8, !tbaa !12
  call void @_ZdlPv(i8* %40) #16
  br label %41

41:                                               ; preds = %25, %39
  call void @llvm.lifetime.end.p0i8(i64 24, i8* nonnull %3) #15
  ret i32 0

42:                                               ; preds = %105, %44
  %43 = phi %"class.std::__1::thread"* [ %45, %44 ], [ %107, %105 ]
  invoke void @_ZNSt3__16thread4joinEv(%"class.std::__1::thread"* nonnull %43)
          to label %44 unwind label %47

44:                                               ; preds = %42
  %45 = getelementptr inbounds %"class.std::__1::thread", %"class.std::__1::thread"* %43, i64 1
  %46 = icmp eq %"class.std::__1::thread"* %45, %108
  br i1 %46, label %23, label %42

47:                                               ; preds = %42
  %48 = landingpad { i8*, i32 }
          cleanup
  %49 = extractvalue { i8*, i32 } %48, 0
  %50 = extractvalue { i8*, i32 } %48, 1
  br label %51

51:                                               ; preds = %47, %20
  %52 = phi i32 [ %21, %20 ], [ %50, %47 ]
  %53 = phi i8* [ %22, %20 ], [ %49, %47 ]
  %54 = getelementptr inbounds %"class.std::__1::vector", %"class.std::__1::vector"* %1, i64 0, i32 0, i32 0
  %55 = load %"class.std::__1::thread"*, %"class.std::__1::thread"** %54, align 8, !tbaa !9
  %56 = icmp eq %"class.std::__1::thread"* %55, null
  br i1 %56, label %70, label %57

57:                                               ; preds = %51
  %58 = bitcast %"class.std::__1::thread"* %55 to i8*
  %59 = load %"class.std::__1::thread"*, %"class.std::__1::thread"** %5, align 8, !tbaa !12
  %60 = icmp eq %"class.std::__1::thread"* %59, %55
  br i1 %60, label %68, label %61

61:                                               ; preds = %57, %61
  %62 = phi %"class.std::__1::thread"* [ %63, %61 ], [ %59, %57 ]
  %63 = getelementptr inbounds %"class.std::__1::thread", %"class.std::__1::thread"* %62, i64 -1
  call void @_ZNSt3__16threadD1Ev(%"class.std::__1::thread"* nonnull %63) #15
  %64 = icmp eq %"class.std::__1::thread"* %63, %55
  br i1 %64, label %65, label %61

65:                                               ; preds = %61
  %66 = bitcast %"class.std::__1::vector"* %1 to i8**
  %67 = load i8*, i8** %66, align 8, !tbaa !9
  br label %68

68:                                               ; preds = %65, %57
  %69 = phi i8* [ %67, %65 ], [ %58, %57 ]
  store %"class.std::__1::thread"* %55, %"class.std::__1::thread"** %5, align 8, !tbaa !12
  call void @_ZdlPv(i8* %69) #16
  br label %70

70:                                               ; preds = %51, %68
  call void @llvm.lifetime.end.p0i8(i64 24, i8* nonnull %3) #15
  %71 = insertvalue { i8*, i32 } undef, i8* %53, 0
  %72 = insertvalue { i8*, i32 } %71, i32 %52, 1
  resume { i8*, i32 } %72

73:                                               ; preds = %11
  %74 = load %"class.std::__1::thread"*, %"class.std::__1::thread"** %5, align 8, !tbaa !12
  %75 = load %"class.std::__1::thread"*, %"class.std::__1::thread"** %6, align 8, !tbaa !8
  %76 = icmp ult %"class.std::__1::thread"* %74, %75
  br i1 %76, label %78, label %77

77:                                               ; preds = %73
  invoke void @_ZNSt3__16vectorINS_6threadENS_9allocatorIS1_EEE21__push_back_slow_pathIS1_EEvOT_(%"class.std::__1::vector"* nonnull %1, %"class.std::__1::thread"* nonnull align 8 dereferenceable(8) %2)
          to label %83 unwind label %16

78:                                               ; preds = %73
  %79 = load i64, i64* %8, align 8, !tbaa !13
  %80 = bitcast %"class.std::__1::thread"* %74 to i64*
  store i64 %79, i64* %80, align 8, !tbaa !13
  store %struct._opaque_pthread_t* null, %struct._opaque_pthread_t** %7, align 8, !tbaa !13
  %81 = getelementptr inbounds %"class.std::__1::thread", %"class.std::__1::thread"* %74, i64 1
  %82 = ptrtoint %"class.std::__1::thread"* %81 to i64
  store i64 %82, i64* %9, align 8, !tbaa !12
  br label %83

83:                                               ; preds = %78, %77
  call void @_ZNSt3__16threadD1Ev(%"class.std::__1::thread"* nonnull %2) #15
  call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %4) #15
  call void @llvm.lifetime.start.p0i8(i64 8, i8* nonnull %4) #15
  invoke void @_Z5spawnv(%"class.std::__1::thread"* nonnull sret align 8 %2)
          to label %84 unwind label %12

84:                                               ; preds = %83
  %85 = load %"class.std::__1::thread"*, %"class.std::__1::thread"** %5, align 8, !tbaa !12
  %86 = load %"class.std::__1::thread"*, %"class.std::__1::thread"** %6, align 8, !tbaa !8
  %87 = icmp ult %"class.std::__1::thread"* %85, %86
  br i1 %87, label %89, label %88

88:                                               ; preds = %84
  invoke void @_ZNSt3__16vectorINS_6threadENS_9allocatorIS1_EEE21__push_back_slow_pathIS1_EEvOT_(%"class.std::__1::vector"* nonnull %1, %"class.std::__1::thread"* nonnull align 8 dereferenceable(8) %2)
          to label %94 unwind label %16

89:                                               ; preds = %84
  %90 = load i64, i64* %8, align 8, !tbaa !13
  %91 = bitcast %"class.std::__1::thread"* %85 to i64*
  store i64 %90, i64* %91, align 8, !tbaa !13
  store %struct._opaque_pthread_t* null, %struct._opaque_pthread_t** %7, align 8, !tbaa !13
  %92 = getelementptr inbounds %"class.std::__1::thread", %"class.std::__1::thread"* %85, i64 1
  %93 = ptrtoint %"class.std::__1::thread"* %92 to i64
  store i64 %93, i64* %9, align 8, !tbaa !12
  br label %94

94:                                               ; preds = %89, %88
  call void @_ZNSt3__16threadD1Ev(%"class.std::__1::thread"* nonnull %2) #15
  call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %4) #15
  call void @llvm.lifetime.start.p0i8(i64 8, i8* nonnull %4) #15
  invoke void @_Z5spawnv(%"class.std::__1::thread"* nonnull sret align 8 %2)
          to label %95 unwind label %12

95:                                               ; preds = %94
  %96 = load %"class.std::__1::thread"*, %"class.std::__1::thread"** %5, align 8, !tbaa !12
  %97 = load %"class.std::__1::thread"*, %"class.std::__1::thread"** %6, align 8, !tbaa !8
  %98 = icmp ult %"class.std::__1::thread"* %96, %97
  br i1 %98, label %100, label %99

99:                                               ; preds = %95
  invoke void @_ZNSt3__16vectorINS_6threadENS_9allocatorIS1_EEE21__push_back_slow_pathIS1_EEvOT_(%"class.std::__1::vector"* nonnull %1, %"class.std::__1::thread"* nonnull align 8 dereferenceable(8) %2)
          to label %105 unwind label %16

100:                                              ; preds = %95
  %101 = load i64, i64* %8, align 8, !tbaa !13
  %102 = bitcast %"class.std::__1::thread"* %96 to i64*
  store i64 %101, i64* %102, align 8, !tbaa !13
  store %struct._opaque_pthread_t* null, %struct._opaque_pthread_t** %7, align 8, !tbaa !13
  %103 = getelementptr inbounds %"class.std::__1::thread", %"class.std::__1::thread"* %96, i64 1
  %104 = ptrtoint %"class.std::__1::thread"* %103 to i64
  store i64 %104, i64* %9, align 8, !tbaa !12
  br label %105

105:                                              ; preds = %100, %99
  call void @_ZNSt3__16threadD1Ev(%"class.std::__1::thread"* nonnull %2) #15
  call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %4) #15
  %106 = getelementptr inbounds %"class.std::__1::vector", %"class.std::__1::vector"* %1, i64 0, i32 0, i32 0
  %107 = load %"class.std::__1::thread"*, %"class.std::__1::thread"** %106, align 8, !tbaa !9
  %108 = load %"class.std::__1::thread"*, %"class.std::__1::thread"** %5, align 8, !tbaa !12
  %109 = icmp eq %"class.std::__1::thread"* %107, %108
  br i1 %109, label %25, label %42
}

declare i32 @__gxx_personality_v0(...)

; Function Attrs: nounwind
declare void @_ZNSt3__16threadD1Ev(%"class.std::__1::thread"*) unnamed_addr #4

declare void @_ZNSt3__16thread4joinEv(%"class.std::__1::thread"*) local_unnamed_addr #5

; Function Attrs: nobuiltin nofree allocsize(0)
declare noalias nonnull i8* @_Znwm(i64) local_unnamed_addr #6

declare void @_ZNSt3__115__thread_structC1Ev(%"class.std::__1::__thread_struct"*) unnamed_addr #5

; Function Attrs: nobuiltin nounwind
declare void @_ZdlPv(i8*) local_unnamed_addr #7

; Function Attrs: ssp uwtable
define internal noalias i8* @"_ZNSt3__114__thread_proxyINS_5tupleIJNS_10unique_ptrINS_15__thread_structENS_14default_deleteIS3_EEEEZ5spawnvE3$_0EEEEEPvS9_"(i8* %0) #1 personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) {
  %2 = invoke nonnull align 8 dereferenceable(8) %"class.std::__1::__thread_specific_ptr"* @_ZNSt3__119__thread_local_dataEv()
          to label %3 unwind label %20

3:                                                ; preds = %1
  %4 = bitcast i8* %0 to %"class.std::__1::__thread_struct"**
  %5 = bitcast i8* %0 to i8**
  %6 = load i8*, i8** %5, align 8, !tbaa !8
  store %"class.std::__1::__thread_struct"* null, %"class.std::__1::__thread_struct"** %4, align 8, !tbaa !8
  %7 = getelementptr inbounds %"class.std::__1::__thread_specific_ptr", %"class.std::__1::__thread_specific_ptr"* %2, i64 0, i32 0
  %8 = load i64, i64* %7, align 8, !tbaa !15
  %9 = invoke i32 @pthread_setspecific(i64 %8, i8* %6)
          to label %12 unwind label %10

10:                                               ; preds = %3
  %11 = landingpad { i8*, i32 }
          cleanup
  br label %25

12:                                               ; preds = %3
  %13 = atomicrmw add i32* getelementptr inbounds ({ { i32 } }, { { i32 } }* @n_threads, i64 0, i32 0, i32 0), i32 1 monotonic
  store i32 %13, i32* @tid, align 4, !tbaa !18
  %14 = tail call i32 (i8*, ...) @printf(i8* nonnull dereferenceable(1) getelementptr inbounds ([4 x i8], [4 x i8]* @.str.1, i64 0, i64 0), i32 %13) #15
  %15 = load %"class.std::__1::__thread_struct"*, %"class.std::__1::__thread_struct"** %4, align 8, !tbaa !8
  store %"class.std::__1::__thread_struct"* null, %"class.std::__1::__thread_struct"** %4, align 8, !tbaa !8
  %16 = icmp eq %"class.std::__1::__thread_struct"* %15, null
  br i1 %16, label %19, label %17

17:                                               ; preds = %12
  tail call void @_ZNSt3__115__thread_structD1Ev(%"class.std::__1::__thread_struct"* nonnull %15) #15
  %18 = bitcast %"class.std::__1::__thread_struct"* %15 to i8*
  tail call void @_ZdlPv(i8* %18) #16
  br label %19

19:                                               ; preds = %12, %17
  tail call void @_ZdlPv(i8* nonnull %0) #16
  ret i8* null

20:                                               ; preds = %1
  %21 = landingpad { i8*, i32 }
          cleanup
  %22 = icmp eq i8* %0, null
  br i1 %22, label %33, label %23

23:                                               ; preds = %20
  %24 = bitcast i8* %0 to %"class.std::__1::__thread_struct"**
  br label %25

25:                                               ; preds = %23, %10
  %26 = phi %"class.std::__1::__thread_struct"** [ %24, %23 ], [ %4, %10 ]
  %27 = phi { i8*, i32 } [ %21, %23 ], [ %11, %10 ]
  %28 = load %"class.std::__1::__thread_struct"*, %"class.std::__1::__thread_struct"** %26, align 8, !tbaa !8
  store %"class.std::__1::__thread_struct"* null, %"class.std::__1::__thread_struct"** %26, align 8, !tbaa !8
  %29 = icmp eq %"class.std::__1::__thread_struct"* %28, null
  br i1 %29, label %32, label %30

30:                                               ; preds = %25
  tail call void @_ZNSt3__115__thread_structD1Ev(%"class.std::__1::__thread_struct"* nonnull %28) #15
  %31 = bitcast %"class.std::__1::__thread_struct"* %28 to i8*
  tail call void @_ZdlPv(i8* %31) #16
  br label %32

32:                                               ; preds = %30, %25
  tail call void @_ZdlPv(i8* nonnull %0) #16
  br label %33

33:                                               ; preds = %20, %32
  %34 = phi { i8*, i32 } [ %21, %20 ], [ %27, %32 ]
  resume { i8*, i32 } %34
}

; Function Attrs: noreturn
declare void @_ZNSt3__120__throw_system_errorEiPKc(i32, i8*) local_unnamed_addr #8

declare i32 @pthread_create(%struct._opaque_pthread_t**, %struct._opaque_pthread_attr_t*, i8* (i8*)*, i8*) local_unnamed_addr #5

declare nonnull align 8 dereferenceable(8) %"class.std::__1::__thread_specific_ptr"* @_ZNSt3__119__thread_local_dataEv() local_unnamed_addr #5

declare i32 @pthread_setspecific(i64, i8*) local_unnamed_addr #5

; Function Attrs: nofree nounwind
declare i32 @printf(i8* nocapture readonly, ...) local_unnamed_addr #9

; Function Attrs: nounwind
declare void @_ZNSt3__115__thread_structD1Ev(%"class.std::__1::__thread_struct"*) unnamed_addr #4

; Function Attrs: inlinehint ssp uwtable
define linkonce_odr void @_ZNSt3__16vectorINS_6threadENS_9allocatorIS1_EEE21__push_back_slow_pathIS1_EEvOT_(%"class.std::__1::vector"* %0, %"class.std::__1::thread"* nonnull align 8 dereferenceable(8) %1) local_unnamed_addr #10 align 2 personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) {
  %3 = getelementptr inbounds %"class.std::__1::vector", %"class.std::__1::vector"* %0, i64 0, i32 0, i32 1
  %4 = bitcast %"class.std::__1::thread"** %3 to i64*
  %5 = load i64, i64* %4, align 8, !tbaa !12
  %6 = bitcast %"class.std::__1::vector"* %0 to i64*
  %7 = load i64, i64* %6, align 8, !tbaa !9
  %8 = sub i64 %5, %7
  %9 = ashr exact i64 %8, 3
  %10 = add nsw i64 %9, 1
  %11 = icmp ugt i64 %10, 2305843009213693951
  %12 = inttoptr i64 %7 to %"class.std::__1::thread"*
  %13 = inttoptr i64 %5 to %"class.std::__1::thread"*
  br i1 %11, label %14, label %16

14:                                               ; preds = %2
  %15 = bitcast %"class.std::__1::vector"* %0 to %"class.std::__1::__vector_base_common"*
  tail call void @_ZNKSt3__120__vector_base_commonILb1EE20__throw_length_errorEv(%"class.std::__1::__vector_base_common"* %15) #17
  unreachable

16:                                               ; preds = %2
  %17 = getelementptr inbounds %"class.std::__1::vector", %"class.std::__1::vector"* %0, i64 0, i32 0, i32 2, i32 0, i32 0
  %18 = bitcast %"class.std::__1::thread"** %17 to i64*
  %19 = load i64, i64* %18, align 8, !tbaa !8
  %20 = sub i64 %19, %7
  %21 = ashr exact i64 %20, 3
  %22 = icmp ult i64 %21, 1152921504606846975
  %23 = ashr exact i64 %20, 2
  %24 = icmp ult i64 %23, %10
  %25 = select i1 %24, i64 %10, i64 %23
  %26 = select i1 %22, i64 %25, i64 2305843009213693951
  %27 = icmp eq i64 %26, 0
  br i1 %27, label %35, label %28

28:                                               ; preds = %16
  %29 = icmp ugt i64 %26, 2305843009213693951
  br i1 %29, label %30, label %31

30:                                               ; preds = %28
  tail call void @_ZNSt3__120__throw_length_errorEPKc(i8* getelementptr inbounds ([68 x i8], [68 x i8]* @.str.2, i64 0, i64 0)) #17
  unreachable

31:                                               ; preds = %28
  %32 = shl nuw i64 %26, 3
  %33 = tail call noalias nonnull i8* @_Znwm(i64 %32) #14
  %34 = bitcast i8* %33 to %"class.std::__1::thread"*
  br label %35

35:                                               ; preds = %31, %16
  %36 = phi %"class.std::__1::thread"* [ %34, %31 ], [ null, %16 ]
  %37 = getelementptr inbounds %"class.std::__1::thread", %"class.std::__1::thread"* %36, i64 %9
  %38 = getelementptr inbounds %"class.std::__1::thread", %"class.std::__1::thread"* %36, i64 %26
  %39 = ptrtoint %"class.std::__1::thread"* %38 to i64
  %40 = getelementptr inbounds %"class.std::__1::thread", %"class.std::__1::thread"* %1, i64 0, i32 0
  %41 = bitcast %"class.std::__1::thread"* %1 to i64*
  %42 = load i64, i64* %41, align 8, !tbaa !13
  %43 = bitcast %"class.std::__1::thread"* %37 to i64*
  store i64 %42, i64* %43, align 8, !tbaa !13
  store %struct._opaque_pthread_t* null, %struct._opaque_pthread_t** %40, align 8, !tbaa !13
  %44 = getelementptr inbounds %"class.std::__1::thread", %"class.std::__1::thread"* %37, i64 1
  %45 = ptrtoint %"class.std::__1::thread"* %44 to i64
  %46 = icmp eq %"class.std::__1::thread"* %13, %12
  br i1 %46, label %60, label %47

47:                                               ; preds = %35, %47
  %48 = phi %"class.std::__1::thread"* [ %50, %47 ], [ %37, %35 ]
  %49 = phi %"class.std::__1::thread"* [ %51, %47 ], [ %13, %35 ]
  %50 = getelementptr inbounds %"class.std::__1::thread", %"class.std::__1::thread"* %48, i64 -1
  %51 = getelementptr inbounds %"class.std::__1::thread", %"class.std::__1::thread"* %49, i64 -1
  %52 = getelementptr inbounds %"class.std::__1::thread", %"class.std::__1::thread"* %51, i64 0, i32 0
  %53 = bitcast %"class.std::__1::thread"* %51 to i64*
  %54 = load i64, i64* %53, align 8, !tbaa !13
  %55 = bitcast %"class.std::__1::thread"* %50 to i64*
  store i64 %54, i64* %55, align 8, !tbaa !13
  store %struct._opaque_pthread_t* null, %struct._opaque_pthread_t** %52, align 8, !tbaa !13
  %56 = icmp eq %"class.std::__1::thread"* %51, %12
  br i1 %56, label %57, label %47

57:                                               ; preds = %47
  %58 = load i64, i64* %6, align 8, !tbaa !8
  %59 = inttoptr i64 %58 to %"class.std::__1::thread"*
  br label %60

60:                                               ; preds = %35, %57
  %61 = phi %"class.std::__1::thread"* [ %59, %57 ], [ %12, %35 ]
  %62 = phi %"class.std::__1::thread"* [ %50, %57 ], [ %37, %35 ]
  %63 = phi i64 [ %58, %57 ], [ %7, %35 ]
  %64 = ptrtoint %"class.std::__1::thread"* %62 to i64
  store i64 %64, i64* %6, align 8, !tbaa !8
  %65 = load %"class.std::__1::thread"*, %"class.std::__1::thread"** %3, align 8, !tbaa !8
  store i64 %45, i64* %4, align 8, !tbaa !8
  store i64 %39, i64* %18, align 8, !tbaa !8
  %66 = icmp eq %"class.std::__1::thread"* %65, %61
  br i1 %66, label %71, label %67

67:                                               ; preds = %60, %67
  %68 = phi %"class.std::__1::thread"* [ %69, %67 ], [ %65, %60 ]
  %69 = getelementptr inbounds %"class.std::__1::thread", %"class.std::__1::thread"* %68, i64 -1
  tail call void @_ZNSt3__16threadD1Ev(%"class.std::__1::thread"* nonnull %69) #15
  %70 = icmp eq %"class.std::__1::thread"* %69, %61
  br i1 %70, label %71, label %67

71:                                               ; preds = %67, %60
  %72 = icmp eq i64 %63, 0
  br i1 %72, label %75, label %73

73:                                               ; preds = %71
  %74 = inttoptr i64 %63 to i8*
  tail call void @_ZdlPv(i8* %74) #16
  br label %75

75:                                               ; preds = %71, %73
  ret void
}

; Function Attrs: noreturn
declare void @_ZNKSt3__120__vector_base_commonILb1EE20__throw_length_errorEv(%"class.std::__1::__vector_base_common"*) local_unnamed_addr #8

; Function Attrs: inlinehint noreturn ssp uwtable
define linkonce_odr hidden void @_ZNSt3__120__throw_length_errorEPKc(i8* %0) local_unnamed_addr #11 personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) {
  %2 = tail call i8* @__cxa_allocate_exception(i64 16) #15
  %3 = bitcast i8* %2 to %"class.std::length_error"*
  invoke void @_ZNSt12length_errorC1EPKc(%"class.std::length_error"* %3, i8* %0)
          to label %4 unwind label %5

4:                                                ; preds = %1
  tail call void @__cxa_throw(i8* %2, i8* bitcast (i8** @_ZTISt12length_error to i8*), i8* bitcast (void (%"class.std::length_error"*)* @_ZNSt12length_errorD1Ev to i8*)) #17
  unreachable

5:                                                ; preds = %1
  %6 = landingpad { i8*, i32 }
          cleanup
  tail call void @__cxa_free_exception(i8* %2) #15
  resume { i8*, i32 } %6
}

declare i8* @__cxa_allocate_exception(i64) local_unnamed_addr

; Function Attrs: ssp uwtable
define linkonce_odr hidden void @_ZNSt12length_errorC1EPKc(%"class.std::length_error"* %0, i8* %1) unnamed_addr #1 align 2 {
  %3 = getelementptr %"class.std::length_error", %"class.std::length_error"* %0, i64 0, i32 0
  tail call void @_ZNSt11logic_errorC2EPKc(%"class.std::logic_error"* %3, i8* %1)
  %4 = getelementptr %"class.std::length_error", %"class.std::length_error"* %0, i64 0, i32 0, i32 0, i32 0
  store i32 (...)** bitcast (i8** getelementptr inbounds ({ [5 x i8*] }, { [5 x i8*] }* @_ZTVSt12length_error, i64 0, inrange i32 0, i64 2) to i32 (...)**), i32 (...)*** %4, align 8, !tbaa !20
  ret void
}

declare void @__cxa_free_exception(i8*) local_unnamed_addr

; Function Attrs: nounwind
declare void @_ZNSt12length_errorD1Ev(%"class.std::length_error"*) unnamed_addr #4

declare void @__cxa_throw(i8*, i8*, i8*) local_unnamed_addr

declare void @_ZNSt11logic_errorC2EPKc(%"class.std::logic_error"*, i8*) unnamed_addr #5

; Function Attrs: norecurse nounwind readnone ssp uwtable
define cxx_fast_tlscc nonnull i32* @_ZTW3tid() local_unnamed_addr #12 {
  ret i32* @tid
}

; Function Attrs: argmemonly nounwind willreturn writeonly
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #13

attributes #0 = { norecurse nounwind readnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { argmemonly nounwind willreturn }
attributes #3 = { norecurse ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { nobuiltin nofree allocsize(0) "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #7 = { nobuiltin nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #8 = { noreturn "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #9 = { nofree nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #10 = { inlinehint ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #11 = { inlinehint noreturn ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #12 = { norecurse nounwind readnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #13 = { argmemonly nounwind willreturn writeonly }
attributes #14 = { builtin allocsize(0) }
attributes #15 = { nounwind }
attributes #16 = { builtin nounwind }
attributes #17 = { noreturn }

!llvm.module.flags = !{!0, !1}
!llvm.ident = !{!2}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{!"clang version 11.1.0"}
!3 = !{!4, !5, i64 0}
!4 = !{!"_ZTSNSt3__122__compressed_pair_elemIPNS_15__thread_structELi0ELb0EEE", !5, i64 0}
!5 = !{!"any pointer", !6, i64 0}
!6 = !{!"omnipotent char", !7, i64 0}
!7 = !{!"Simple C++ TBAA"}
!8 = !{!5, !5, i64 0}
!9 = !{!10, !5, i64 0}
!10 = !{!"_ZTSNSt3__113__vector_baseINS_6threadENS_9allocatorIS1_EEEE", !5, i64 0, !5, i64 8, !11, i64 16}
!11 = !{!"_ZTSNSt3__117__compressed_pairIPNS_6threadENS_9allocatorIS1_EEEE"}
!12 = !{!10, !5, i64 8}
!13 = !{!14, !5, i64 0}
!14 = !{!"_ZTSNSt3__16threadE", !5, i64 0}
!15 = !{!16, !17, i64 0}
!16 = !{!"_ZTSNSt3__121__thread_specific_ptrINS_15__thread_structEEE", !17, i64 0}
!17 = !{!"long", !6, i64 0}
!18 = !{!19, !19, i64 0}
!19 = !{!"int", !6, i64 0}
!20 = !{!21, !21, i64 0}
!21 = !{!"vtable pointer", !7, i64 0}
