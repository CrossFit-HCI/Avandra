const {getDefaultConfig, mergeConfig} = require('@react-native/metro-config');

const packagePath =
    '/Users/heades/development/fp-lib';

/**
 * Metro configuration
 * https://facebook.github.io/metro/docs/configuration
 *
 * @type {import('metro-config').MetroConfig}
 */
const config = {
    resolver: {
        nodeModulesPaths: [packagePath],
        unstable_enableSymlinks: true,
    },
};

module.exports = mergeConfig(getDefaultConfig(__dirname), config);