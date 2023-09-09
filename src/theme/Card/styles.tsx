import { StyleSheet } from 'react-native';
import { borderTheme, iconTheme } from '../../designFramework/theme';

export const iconStyle = {
    size: iconTheme.secondarySize,
    completedIcon: iconTheme.completed,
    uncompletedIcon: iconTheme.uncompleted
};

export const cardStyle = StyleSheet.create({
    component: {
        flex: 1,
        width: '90%',
        maxHeight: '82%',
        marginTop: 15,
        borderColor: borderTheme.card.borderColor,
        borderWidth: borderTheme.card.borderWidth
    },
    headerComponent: {
        flex: 1,
        flexDirection: 'row',
        justifyContent: 'center',
        alignItems: 'center',        
        maxHeight: 40,        
        backgroundColor: 'rgba(196,196,196,1)',
        borderColor: borderTheme.card.borderColor,
        borderWidth: borderTheme.card.borderWidth
    },
    headerTextForecastContainer: {
        flex: 1,
        flexDirection: 'row',
        justifyContent: 'center'
    },
    headerTextContainer: {
        flex: 1,
        padding: 5,
        flexDirection: 'row',
        justifyContent: 'flex-start'            
    },
    headerText: {
        fontSize: 22,
        fontWeight: 'bold'
    },
    completedContainer: {
        flex: 1,
        maxWidth: 50,
        height: '100%',
        alignItems: 'center',
        justifyContent: 'center'
    },
    headerButtonStyle: {
        flex: 1,
        height: '100%',
        maxWidth: 70,
        backgroundColor: 'black',
        justifyContent: 'center',
        alignItems: 'center',
        elevation: 1
    },
    headerButtonTextStyle: {
        color: 'white',
        fontSize: 25
    }
});
