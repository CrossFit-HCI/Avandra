module.exports = {
  parser: '@typescript-eslint/parser',
  plugins: ['@typescript-eslint'],
  root: true,
  extends: ['plugin:@typescript-eslint/recommended'],
  rules: {
    semi: 'error',
    '@typescript-eslint/semi': 'error',
    quotes: ['error', 'single'],
  },
};
