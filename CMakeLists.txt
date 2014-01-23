cmake_minimum_required(VERSION 2.8.7)
include($ENV{ROS_ROOT}/core/rosbuild/rosbuild.cmake)

rosbuild_init()
rosbuild_include(rosprolog utils)

set(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/bin)
set(LIBRARY_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/lib)
set(JAVA_OUTPUT_DIR ${PROJECT_SOURCE_DIR}/bin)

add_classpath(${PROJECT_SOURCE_DIR}/lib/mongo-java-driver-2.11.3.jar)
add_runtime_classpath(${PROJECT_SOURCE_DIR}/lib/mongo-java-driver-2.11.3.jar)

add_classpath(${PROJECT_SOURCE_DIR}/lib/java-json.jar)
add_runtime_classpath(${PROJECT_SOURCE_DIR}/lib/java-json.jar)


add_java_source_dir(${PROJECT_SOURCE_DIR}/src)

rosbuild_find_ros_package(mongo_jpl)
set( LOCAL_PACKAGE_PATH ${mongo_jpl_PACKAGE_PATH} )
fix_ros_destination_path(LOCAL_PACKAGE_PATH)

rospack_add_java_executable(MongoPrologInterface mongo_jpl.MongoPrologInterface)
