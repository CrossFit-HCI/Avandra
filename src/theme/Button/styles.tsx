import { StyleSheet } from 'react-native';
import { colorDF } from '../../designFramework/Color';

export const controlButtonLargeStyle = StyleSheet.create({
    container: {
        height: 83,
        width: 83,
        backgroundColor: colorDF.button.primaryBackground,
        justifyContent: 'center',
        alignItems: 'center',
        elevation: 1
    },
    text: {
        color: colorDF.button.primaryFont,
        fontSize: 18,        
    }
});

export const controlButtonStyle = StyleSheet.create({
    container: {
        flex: 1,
        height: 30,
        width: 78,
        backgroundColor: colorDF.button.primaryBackground,
        justifyContent: 'center',
        alignItems: 'center',
        elevation: 1
    },
    text: {
        color: colorDF.button.primaryFont,
        fontSize: 18,        
    }
});
