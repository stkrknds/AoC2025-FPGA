# Makefile for Xilinx xsim simulation

# Configuration
TOP_MODULE = tb_top
RTL_DIR = rtl
TB_DIR = tb
TCL_SCRIPT = run_sim.tcl
PROJECT_DIR = sim_project

# Find all source files
RTL_FILES = $(wildcard $(RTL_DIR)/*.v $(RTL_DIR)/*.sv)
TB_FILES = $(wildcard $(TB_DIR)/*.v $(TB_DIR)/*.sv)
ALL_FILES = $(RTL_FILES) $(TB_FILES)

.PHONY: all sim sim_standalone solve clean help

# Default target - use Vivado batch mode
all: sim

# Run only top_tb.sv testbench
solve: $(ALL_FILES)
	@echo "=== Running top_tb simulation ==="
ifdef file_path
	@echo "Using file path: $(file_path)"
	vivado -mode batch -source run_solve.tcl -tclargs $(file_path)
else
	@echo "No file path provided, using default"
	vivado -mode batch -source run_solve.tcl
endif

# Run simulation using Vivado batch mode with TCL script
sim: $(ALL_FILES)
	@echo "=== Running simulation via Vivado ==="
	vivado -mode batch -source $(TCL_SCRIPT)

# Alternative: Run simulation using standalone xsim commands
sim_standalone: $(ALL_FILES)
	@echo "=== Analyzing RTL files ==="
	@for file in $(RTL_FILES); do \
		echo "Compiling $$file"; \
		xvlog $$file || exit 1; \
	done
	@echo "=== Analyzing testbench files ==="
	@for file in $(TB_FILES); do \
		echo "Compiling $$file"; \
		xvlog $$file || exit 1; \
	done
	@echo "=== Elaborating design ==="
	xelab -debug typical -top $(TOP_MODULE) -snapshot sim_snapshot
	@echo "=== Running simulation ==="
	xsim sim_snapshot -runall -nolog

# Run simulation with GUI using standalone xsim
gui_standalone:
	@echo "=== Running simulation with GUI ==="
	@for file in $(RTL_FILES); do xvlog $$file || exit 1; done
	@for file in $(TB_FILES); do xvlog $$file || exit 1; done
	xelab -debug typical -top $(TOP_MODULE) -snapshot sim_snapshot
	xsim sim_snapshot -gui

# Clean generated files
clean:
	rm -rf $(PROJECT_DIR)
	rm -rf xsim.dir
	rm -rf *.jou *.log *.pb *.wdb *.str
	rm -rf .Xil
	rm -rf vivado*

# Help
help:
	@echo "Makefile targets:"
	@echo "  make sim            - Run ALL testbenches via Vivado batch mode (default)"
	@echo "  make solve          - Run ONLY top_tb.sv testbench"
	@echo "  make solve file_path=<path> - Run top_tb.sv with custom file path"
	@echo "  make sim_standalone - Run simulation with standalone xsim (no Vivado)"
	@echo "  make gui_standalone - Run simulation with GUI using standalone xsim"
	@echo "  make clean          - Remove generated files"
	@echo "  make help           - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make solve file_path=../data/test.txt"
	@echo "  make solve file_path=/absolute/path/to/input.dat"
	@echo ""
	@echo "Configuration:"
	@echo "  TOP_MODULE = $(TOP_MODULE)"
	@echo "  RTL_DIR    = $(RTL_DIR)"
	@echo "  TB_DIR     = $(TB_DIR)"
