# ----------------------------------------
# 定义伪目标
.PHONY: all compile application clean

# ----------------------------------------
# Erlang 后缀规则
.SUFFIXES: .erl .beam .yrl

# 普通 Erlang 编译规则
# 如果你需要生成一个 .beam 文件，而它对应的 .erl 文件存在，使用下面的命令生成
.erl.beam:
	erlc -W $<

# Yecc 文件编译规则（如果有）
.yrl.erl:
	erlc -W $<

# Erlang 启动命令
# ERL = erl -boot start_clean
ERL = erl

# ----------------------------------------
# 模块目录列表（根据项目结构修改）
# 此处的相对路径是与makefile文件一致的
SRC_DIR  = src
EBIN_DIR = ebin

# 所有源文件 
# SOURCES = $(foreach dir,$(SRC_DIR),$(shell find $(dir) -name "*.erl"))    # linux版本
# SOURCES = $(foreach dir,$(SRC_DIR),$(wildcard $(dir)/**/*.erl))      # windows版本
# 优化，上面的两种做法会漏掉 src 目录下同一层级的直接的 .erl 文件（不在子目录中的那些）。
# src/foo.erl             没被匹配到
# src/subdir/bar.erl      被匹配到了
# SOURCES = $(foreach dir,$(SRC_DIR),$(wildcard $(dir)/*.erl) $(wildcard $(dir)/**/*.erl))
SOURCES := $(shell find $(SRC_DIR) -type f -name "*.erl")

# ① 生成对应的 .beam 文件路径（保持目录结构）并存放在ebin目录下  例如/src/temp/test.erl --> /ebin/temp/test.beam
# BEAMS = $(patsubst $(SRC_DIR)/%.erl,$(EBIN_DIR)/%.beam,$(SOURCES))
# $(EBIN_DIR)/%.beam: $(SRC_DIR)/%.erl
# 	@mkdir -p $(dir $@)
# 	erlc -W -o $(dir $@) $<


# ② 生成对应的 .beam文件路径 (不保持与src的目录结构一致，全都放到ebin目录之下，无子目录)
BEAMS = $(patsubst %.erl,$(EBIN_DIR)/%.beam,$(notdir $(SOURCES)))
$(EBIN_DIR)/%.beam: 
	@mkdir -p $(EBIN_DIR)
	@src_file=$$(find $(SRC_DIR) -type f -name "$(basename $(notdir $@)).erl"); \
	if [ -n "$$src_file" ]; then \
		echo "Compiling $$src_file → $(EBIN_DIR)/"; \
		erlc -W -o $(EBIN_DIR) $$src_file; \
	else \
		echo "Source file for $@ not found."; \
		exit 1; \
	fi


# 特殊文件单独编译
# specia1.beam: src/special/specia1.erl
# 	erlc -Dflag1 -W0 $<

# ----------------------------------------
# 默认目标
all: compile

# 编译所有模块
compile: $(BEAMS)

# ----------------------------------------
# 启动应用
# 自动把模块目录加入到代码载入器的搜索路径列表中
PA_PATHS = -pa $(EBIN_DIR)

application: compile
	$(ERL) $(PA_PATHS) -s hello world

# ----------------------------------------
# 清理
# 直接删除整个ebin目录，下次启动全部重新编译
clean:
	@echo "Cleaning up ebin directory..."
	@if [ -d $(EBIN_DIR) ]; then \
		rm -rf $(EBIN_DIR) \
		echo "✅ Clean complete."; \
	else \
		echo "⚠️ No ebin directory found."; \
	fi
	@rm -f erl_crash.dump



