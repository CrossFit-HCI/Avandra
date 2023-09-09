module.exports = {
  parser: '@typescript-eslint/parser',
  plugins: ['@typescript-eslint'],
  root: true,
  extends: ['plugin:@typescript-eslint/recommended'],
  rules: {
    semi: 'off',
    '@typescript-eslint/semi': 'error',
    quotes: 'off',
    '@typescript-eslint/quotes': ['error', 'single'],
    'space-infix-ops': 'off',
    '@typescript-eslint/space-infix-ops': 'error',
    '@typescript-eslint/type-annotation-spacing': ['error', {'after':true}],
    '@typescript-eslint/no-explicit-any': 'error',
    '@typescript-eslint/no-inferrable-types': 'off',
    'no-extra-semi': 'off',
    '@typescript-eslint/no-extra-semi': 'error'
  },
};
