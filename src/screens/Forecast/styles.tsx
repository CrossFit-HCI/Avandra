import { StyleSheet } from 'react-native';

import { colorTheme, fontTheme } from '../../designFramework/theme';

import { cardStyle } from '../../theme/Card/styles';

export { iconStyle  } from '../../theme/Card/styles';

export const forecastStyle = StyleSheet.create({
        component: {
            flex: 1,
            justifyContent: 'flex-start',
            paddingTop: 30,
            alignItems: 'center',
            backgroundColor: colorTheme.component.background,
            card: cardStyle
        },

        heading1: {
            color: fontTheme.heading1Font.color,
            fontSize: fontTheme.heading1Font.size
        }
    });
