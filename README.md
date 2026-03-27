# 我的 Doom Emacs 配置

这是我的私有 Doom Emacs 配置仓库，当前按 **`~/.config/doom`** 目录布局维护，并通过 `modules/cnsunyour/` 下的私有模块扩展 Doom 的默认能力。

## 安装与初始化

### 1. 准备依赖

至少需要：
- Emacs
- Git
- `ripgrep`
- `fd`

在 macOS 上可按自己的习惯安装 Emacs；这个仓库当前主要面向 macOS + Homebrew 环境使用。

### 2. 安装 Doom Emacs

```shell
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
```

### 3. 克隆本仓库

```shell
git clone git@github.com:cnsunyour/.doom.d.git ~/.config/doom
```

虽然仓库名仍是 `.doom.d`，但当前实际使用的目录是 `~/.config/doom`。

### 4. 初始化 Doom

首次安装：

```shell
~/.config/emacs/bin/doom install
```

如果 Doom 已经安装过，只是同步当前配置：

```shell
~/.config/emacs/bin/doom sync
```

### 5. 启动 Emacs

完成后直接启动 Emacs 即可。

## 日常维护命令

- 同步配置：

  ```shell
  ~/.config/emacs/bin/doom sync
  ```

  修改 `init.el`、`packages.el`、模块包声明或 autoload 相关内容后，优先运行这个命令。

- 检查环境与常见问题：

  ```shell
  ~/.config/emacs/bin/doom doctor
  ```

- 更新 Doom 框架与包：

  ```shell
  ~/.config/emacs/bin/doom upgrade
  ```

- 重新生成 GUI 环境变量：

  ```shell
  ~/.config/emacs/bin/doom env
  ```

  如果 Emacs 启动后找不到 shell 中可用的命令（如 `python`、`node`、`rg`），通常先跑这个。

## 仓库结构

### 根入口文件

- `init.el`：控制 Doom 标准模块和私有 `:cnsunyour` 模块是否启用
- `config.el`：全局配置、环境变量、跨模块行为、私有文件加载
- `packages.el`：顶层额外包声明

### 私有模块

主要功能放在 `modules/cnsunyour/` 下，当前启用的私有模块包括：

- `bindings`
- `blog`
- `calendar`
- `chinese`
- `editor`
- `org`
- `telega`
- `term`
- `tools`
- `ui`
- `ai`
- `love`

这些模块通常以 `config.el` + `packages.el` 的方式组织，部分模块还会拆出 `+*.el` 或 `autoload/*.el` 辅助文件。

## 本地私有文件

仓库根目录下有两个本地覆盖文件：

- `.private.el`：放机器私有配置、凭据、只适用于当前机器的值
- `.custom.el`：Emacs Customize 的输出文件

它们都已经被 `.gitignore` 忽略，不会提交到仓库。

### 使用约定

- 优先把敏感信息放进 `.private.el`
- 尽量不要让凭据或 token 写进 `.custom.el`
- `init.el` 已把 `custom-file` 重定向到 `.custom.el`
- `config.el` 会显式加载 `.private.el` 和 `.custom.el`

如果你调整了这套机制，请同时检查启动加载逻辑是否仍然成立。

## 验证改动是否正常

这个仓库没有独立的单元测试或 lint 流程，通常按下面方式验证：

1. 运行：

   ```shell
   ~/.config/emacs/bin/doom sync
   ```

2. 再运行：

   ```shell
   ~/.config/emacs/bin/doom doctor
   ```

3. 根据改动范围做最小回归：
   - 检查对应 mode 的 hook 是否触发
   - 检查相关 keybinding 是否仍可用
   - 检查 popup / side window 规则是否正常
   - 检查延迟加载是否仍按预期工作

## 旧路径说明

这个仓库早期文档使用过以下旧约定：
- `~/.doom.d`
- `~/.emacs.d/bin/doom re`

当前维护时请统一使用：
- Doom 配置目录：`~/.config/doom`
- Doom CLI：`~/.config/emacs/bin/doom`
- 日常同步命令：`doom sync`

如果你的机器上同时存在 `~/.doom.d` 和 `~/.config/doom`，请确认实际生效的是哪一套配置。