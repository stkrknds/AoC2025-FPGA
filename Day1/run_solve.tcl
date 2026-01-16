# Xilinx xsim TCL simulation script for top_tb only
# Compiles all RTL modules but runs only top_tb.sv

# Configuration
set TOP_TB "top_tb"
set RTL_DIR "rtl"
set TB_DIR "tb"
set TB_FILE "tb/top_tb.sv"

# Get file path from command line arguments
set file_path ""
if {$argc > 0} {
    set file_path [lindex $argv 0]
    puts "File path argument received: $file_path"
} else {
    puts "No file path provided, testbench will use default"
}

# Get all Verilog/SystemVerilog files
set rtl_files [glob -nocomplain -directory $RTL_DIR *.v *.sv]
set tb_files [glob -nocomplain -directory $TB_DIR *.v *.sv]

# Combine all source files
set all_files [concat $rtl_files $tb_files]

if {[llength $all_files] == 0} {
    puts "ERROR: No source files found in $RTL_DIR or $TB_DIR"
    exit 1
}

# Check if top_tb.sv exists
if {![file exists $TB_FILE]} {
    puts "ERROR: $TB_FILE not found"
    exit 1
}

puts "Found [llength $all_files] source files"
puts "Running testbench: $TOP_TB from $TB_FILE"

# Change to simulation directory
file mkdir sim_build
cd sim_build

# Compile all files
puts "\n=== Compiling source files ==="
foreach file $all_files {
    set abs_file [file normalize ../$file]
    puts "Compiling: $file"
    if {[catch {exec xvlog -sv $abs_file} result]} {
        puts "ERROR: Compilation failed for $file"
        puts $result
        cd ..
        exit 1
    }
}

# Elaborate the top_tb design
puts "\n=== Elaborating $TOP_TB ==="
set snapshot "${TOP_TB}_snapshot"
if {[catch {exec xelab -debug typical -top $TOP_TB -snapshot $snapshot} result]} {
    puts "ERROR: Elaboration failed for $TOP_TB"
    puts $result
    cd ..
    exit 1
}

# Run simulation with parameters
puts "\n=== Running $TOP_TB simulation ==="

# Pass file path to SystemVerilog if provided
if {$file_path != ""} {
    if {[catch {exec xsim $snapshot -testplusarg "FILE_PATH=$file_path" -runall -nolog} result]} {
        puts "ERROR: Simulation failed"
        puts $result
        cd ..
        exit 1
    } else {
        puts $result
    }
} else {
    if {[catch {exec xsim $snapshot -runall -nolog} result]} {
        puts "ERROR: Simulation failed"
        puts $result
        cd ..
        exit 1
    } else {
        puts $result
    }
}

cd ..
puts "\n$TOP_TB simulation completed successfully"
exit 0
