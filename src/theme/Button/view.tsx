import { GestureResponderEvent, Pressable, StyleProp, Text, TextStyle, ViewStyle } from 'react-native';
import { controlButtonLargeStyle, controlButtonStyle } from './styles';

interface ControlButtonUnstyleProps {
    /** The title of the button. */
    title: string,
     /** The call back for when the button is pressed. */
     onPress: (event: GestureResponderEvent) => void,
     /** Stylesheet for the button. */
     containerStyle?: StyleProp<ViewStyle>,
     textStyle?: StyleProp<TextStyle>
}

const ControlButtonUnstyled = (props: ControlButtonUnstyleProps) => {    
    return (
        <Pressable onPress={props.onPress} style={props.containerStyle}>
            <Text style={props.textStyle}>{props.title}</Text>
        </Pressable>
    );
};

export interface ControlButtonProps {
    /** The title of the button. */
    title: string,
     /** The call back for when the button is pressed. */
     onPress: (event: GestureResponderEvent) => void
}

export const ControlButton = (props: ControlButtonProps) => {    
   return (
    <ControlButtonUnstyled 
        title={props.title}
        onPress={props.onPress}
        containerStyle={controlButtonStyle.container}
        textStyle={controlButtonStyle.text}
    />
   );
};

export const ControlButtonLarge = (props: ControlButtonProps) => {    
    return (
     <ControlButtonUnstyled 
         title={props.title}
         onPress={props.onPress}
         containerStyle={controlButtonLargeStyle.container}
         textStyle={controlButtonLargeStyle.text}
     />
    );
 };