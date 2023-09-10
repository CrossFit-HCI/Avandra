import { StyleSheet } from 'react-native';
import { colorDF } from '../../designFramework/DF';

export const hruleStyle = StyleSheet.create({
    component: {
        backgroundColor: colorDF.border.primary, 
        width: '100%', 
        height: 1
    }
});