# oh-my-custom-zsh

Customized [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh).

![common](images/common.png)
![clean](images/clean.png)
![dirty](images/dirty.png)

## Installation

  ```
  cd /path1
  git clone https://github.com/robbyrussell/oh-my-zsh.git
  cd /path2
  git clone https://github.com/xandeer/oh-my-custom-zsh.git
  ```
Open /path2/oh-my-custom-zsh/zshrc, modify `ZSH` and `ZSH_CUSTOM` to /path1/oh-my-zsh and /path2/oh-my-custom-zsh. Make a soft link for /path2/oh-my-custom-zsh/zshrc to ~/.zshrc.

```
ln -s /path2/oh-my-custom-zsh/zshrc ~/.zshrc
```
