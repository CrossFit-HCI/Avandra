import { StyleSheet } from 'react-native';
import { colorTheme } from '../../designFramework/theme';

export const hruleStyle = StyleSheet.create({
    component: {
        backgroundColor: colorTheme.border.primary, 
        width: '100%', 
        height: 1
    }
});