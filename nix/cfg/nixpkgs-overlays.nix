[(self: super: {
  emacs26 = super.emacs26.override {
    imagemagick = self.imagemagickBig;
  };
})]
