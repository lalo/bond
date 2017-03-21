add_bond_test (grpc_pingpong pingpong.bond pingpong.cpp GRPC)

target_compile_definitions (grpc_pingpong PRIVATE -D_WIN32_WINNT=0x0600)
